DB_PSA <- function(continue_previous = F,
                   temp_init = 3,
                   t_damp = 0.75,
                   sched_type = 'exponential',
                   hyper = F,
                   nTweak = 5,
                   total_servers,
                   sim_length,
                   warmup,
                   obj_function_list,
                   optim_type,
                   nVar,
                   init_sol = NA,
                   print_it_results = T,
                   results_directory = NA,
                   use_test_bench = T,
                   inverted_V_logical = T,
                   generate_plots = T) {
  # This function implements the Distance-Based Pareto Simulated-Annealing optimization algorithm
  
  # Function Inputs:
  #   temp_init - initial temperature
  #   t_damp: temperature modification parameter, effect of this parameter depends on the chosen cooling schedule
  #   sched_type - hyper parameter indicating how the temperature changes over time. Four schedules are currently available (linear, quadratic, exponential and logarithmic)
  #   nTweak - maximum number of candidate solutions generated during an iteration
  #   print_it_results - logical indicating whether to print individual iteration results to the console
  #   total_servers - decision variable upper bound
  #   warmup - How long before the DES reaches steady state
  #   sim_length - How long should the DES record statistics after the warmup period
  #   obj_function_list - character vector of functions used as objective functions
  #   optim_type - character vector indicating if the corresponding objective function is maximized ("max") or minimized ("min")
  #   nVar - number of decision variables
  #   init_sol - a starting solution.
  #   generate_plots - logical indicating whether to produce plots of the Pareto image as it evolves
  
  # Initialize Algorithm Hyper-parameters  ---------------------------------------------------------------------------
  if (any(is.na(init_sol))) {
    init_sol <- c(1, rep(0, nVar - 1))
  }
  
  # Set up plotd directory
  if (!is.na(results_directory)) {
    if (!dir.exists(results_directory)) {
      dir.create(results_directory)
    }
    plot_dir <- file.path(results_directory, 'Plots')
    if (!dir.exists(plot_dir) & generate_plots) {
      dir.create(plot_dir)
    }
  }
  
  cat('Schedule =',
      sched_type,
      '\n',
      'Alpha =',
      t_damp,
      '\n',
      'nTweak =',
      nTweak,
      '\n')
  
  # Set the cooling schedule from function inputs
  quad_logical <-
    linear_logical  <- log_logical  <- exp_logical <- F
  if (sched_type == 'quadratic') {
    quad_logical  <- T
  } else if (sched_type == 'linear') {
    linear_logical  <- T
  } else if (sched_type == 'log_mult') {
    log_logical  <- T
  } else if (sched_type == 'exponential') {
    exp_logical <- T
  }
  cool_env <- environment()
  reduce_temp  <-
    function()
      cool_temp(cool_sched = sched_type, .envir = cool_env)
  
  if (!continue_previous) {
    temp <- temp_init
    t_min <- .01 * temp_init
    best_limit <- 5
    pareto_limit <- 25
    starter_reps <- 8
    stat_logical <- T
    maxChange <- 1
    itReps_Cum  <- 0
    nTotal_Cum <- 0
    nTweak <- 5
    itMax <- 100
    best_counter <- 0
    delta <- max(ceiling(nTweak / 2), 10)
    A <- list()
    mosa_trials <- 3
    i <- it <- 1
    seed_val <- floor(runif(1) * 10000)
    analytical <- F
    num_obj <- length(obj_function_list)
    best_by_min_dist <- T
    
    # Generate Initial Baseline Solution ------------------------------------------------------------------------------
    initial <- list(
      name = 'Baseline',
      Solution = init_sol,
      Replications = 12,
      # Should == reps argument from CostFunction function (MOSA Functions.R line 150)
      Allocation = decode(init_sol),
      counter = 0,
      Dist = 0,
      deltaPsi = 0
    )
    #debug(objective_Metrics)
    init_data <-
      objective_Metrics(x = CostFunction(sol = initial$Allocation,
                                         logic = F))
    
    initial <-
      updateSimStats(i = initial,
                     data = init_data,
                     new_sol = T)
    best <- initial
    
    # Initialize Dataframe of Best Solutions---------------------------------------------------------------------------
    best_df <-
      data.table(
        Iteration = 0,
        best$Obj_mean,
        Num_Replications = best$Cost[, max(replication)],
        Dist = NA_real_,
        Pr_Select = NA_real_
      )
    
    itRepsDF <-
      data.table(
        Iteration = 0,
        MOCBA = 0,
        Theoretical = 0,
        Temperature = temp_init,
        Exec_Time = NA_real_
      )
    
    all_allocations <<- t(best$Allocation)
    pareto_set <-  list()
  }
  # Main Optimization Algorithm Loop ---------------------------------------------------------------------------------
  while (termination_criteria(check_pareto = T)) {
    now <- Sys.time()
    extraReps <-
      F # Conditional for if extra simulation replications were used in the MOCBA
    # Generate Candidates for the Iteration ---------------------------------------------------------------------------
    temp_obj <- gen_candidates()
    if (length(temp_obj) != 0) {
      # Perform OCBA to Minimize Total Simulation Replications ----------------------------------------------------------
      temp_obj <- ocba(candidate_list = temp_obj)
      ocba_removed <- temp_obj[['non_sP']]
      ocba_removed <-
        ocba_removed[which(lapply(ocba_removed, length) != 0)]
      temp_obj <- temp_obj[['sP']]
      
      if (it >= 1) {
        prev_pareto_set <- pareto_set
      }
      
      # Update the estimated Pareto set with newly generated candidates ----------------------------------------------------------
      pareto_set <-
        updateParetoSet(pSet = pareto_set, candidate_list =  temp_obj)
      
      ranks <- pareto_set$ranks
      pareto_set <- pareto_set$pSet
      
      if (length(pareto_set) > 1) {
        pareto_order <-
          data.table(name = pareto_set %c% 'name', t(pareto_set %c% 'Obj_mean'))[, .(apply(.SD, 2, unlist))]
        pareto_order <-
          pareto_order[, setdiff(colnames(pareto_order), 'name') := lapply(.SD, as.numeric), .SDcols = setdiff(colnames(pareto_order), 'name')]
        pareto_order <-
          pareto_order[, (setdiff(colnames(pareto_order), 'name')[grep('min', optim_type)]) := lapply(
            pareto_order = .SD,
            FUN = function(i)
              i * -1
          ), .SDcols = setdiff(colnames(pareto_order), 'name')[grep('min', optim_type)], by = name]
        setkeyv(pareto_order, setdiff(colnames(pareto_order), 'name'))
        pareto_set <-
          pareto_set[match(pareto_order$name, pareto_set %c% 'name')]
      }
      
      # Finding the "best" solution to be modified in the next iteration ----------------------------------------------------------
      prev_best <- best
      ret_list <- findBestbyDistance(pSet = pareto_set)
      pareto_set <- ret_list$pareto_set
      best <- ret_list$best
      
      # Update the history list with rejected solutions and the current pareto set ----------------------------------------------------------
      A <-
        updateHistory(candidate_list = append(temp_obj, ocba_removed))
      
      # Kill Zombie Processes leftover from MOCBA
      if (!any(sapply(X = c('Linux', 'Darwin'), function(i)
        i == Sys.info()['sysname']))) {
        reap_zombies() # Kill Zombie processes
      }
      
      # Update Iteration Best and Iteration Info Dataframes -------------------------------------------------------------
      if (it == 1) {
        itReps_Cum <- itReps
        theoretical_cum <- N_Total
        pareto_counter <- 0
      } else{
        itReps_Cum %+% itReps
        theoretical_cum %+% N_Total
        if (identical(pareto_set %c% 'name', prev_pareto_set %c% 'name')) {
          pareto_counter %+% 1
        } else {
          pareto_counter <- 0
        }
      }
      
      best <- `if`(length(best) == 1, best[[1]], best)
      best_df <- rbindlist(list(
        best_df,
        data.table(
          Iteration = it,
          best$Obj_mean,
          Num_Replications = best$Replications,
          Dist = best$Divergence,
          Pr_Select = best$P_Selection
        )
      ), fill = T)
      
      itRepsDF <-
        rbindlist(list(
          itRepsDF,
          data.table(
            Iteration = it,
            MOCBA = itReps_Cum,
            Theoretical = theoretical_cum,
            Temperature = temp,
            Exec_Time = as.numeric(Sys.time() - now)
          )
        ))
      
      # Code removed for now
      all_allocations <-
        rbind(all_allocations, t(temp_obj %c% 'Allocation'))
      
      # if(length(pareto_set) > 1 & length(optim_type) == 2){
      #   plotParetoFront(inputData = pareto_set)
      # }
      if (print_it_results) {
        cat('Iteration',
            it,
            'required',
            itReps,
            'simulation replications.')
        cat('\n')
        cat(
          'The temperature is now',
          temp,
          'the Pr{Acceptance} is',
          p_accept(temp),
          'and',
          gsub(
            pattern = '_',
            replacement = " ",
            x = best$name
          ),
          'moves on.'
        )
        cat('\n')
        cat('The ideal point is', paste(g_ideal_CI, collapse = ', '))
        cat('\n')
        cat('There are',
            length(pareto_set),
            'solutions in the Pareto Set')
        cat('\n')
        if (extraReps) {
          cat('An extra',
              itReps - candidate_reps,
              'replications were used during MOCBA \n')
        }
        cat('\n')
        if (length(pareto_set) > 1) {
          print(rbindlist(lapply(pareto_set, function(i)
            data.table(
              name = i$name,
              p_selection = i$P_Selection,
              divergence = i$Divergence,
              i$Obj_CI
            ))))
        }
        cat('\n')
      }
      if (!is.na(results_directory)) {
        save.image(file = file.path(results_directory, 'paused_envr.rdata'))
      }
      it %+% 1
      
      # Adjust Temperature ----------------------------------------------------------------------------------------------
      if (!identical(pareto_set, prev_pareto_set)) {
        temp <- reduce_temp()
        if (length(pareto_set) > 1. & generate_plots) {
          jpeg(file = file.path(
            plot_dir,
            paste0('Iteration_', it, '_pareto_image.jpeg')
          ))
          plotParetoFront(pareto_set)
          dev.off()
        }
      }
    } else {
      break
    }
    
  }
  if (hyper) {
    
    res_list <- list(
      'sched_type' = sched_type,
      't_damp' = t_damp,
      'nTweak' = nTweak,
      'total_iterations' = it,
      'nReplications' = itReps_Cum,
      'pSet' = pareto_set
    )
    
    saveRDS(res_list, file = file.path(results_directory, paste0(
      paste(sched_type, t_damp, nTweak, sep = '_'), '.rds'
    )))
    return(res_list)
  }
}