DB_PSA <- function(continue_previous = F,
                   temp_init = 3,
                   t_damp = 0.75,
                   sched_type = 'e',
                   nTweak = 5,
                   pareto_limit = NA_real_,
                   itMax = NA_real_,
                   total_servers,
                   sim_length,
                   warmup,
                   starter_reps = 8,
                   obj_function_list,
                   optim_type,
                   nVar,
                   init_sol = NA,
                   print_it_results = T,
                   results_directory = NA,
                   use_test_bench = T,
                   inverted_V_logical = T,
                   hyper = F,
                   generate_plots = T,
                   ...) {
  
  # Implements the Distance-Based Pareto Simulated-Annealing optimization algorithm
  
  # Function Inputs:
  #   temp_init - (Numeric) initial temperature
  #   t_damp: (Numeric) temperature modification parameter, effect of this parameter depends on the chosen cooling schedule
  #   sched_type - (Character) hyper parameter indicating how the temperature changes over time. 
  #                 Four schedules are currently available: 
  #                           - linear ("linear" or "l")
  #                           - quadratic ("quadratic" or "q")
  #                           - exponential ("exponential" or "e) 
  #                           - logarithmic ("log_mult" or "r")
  #   pareto_limit - (Integer) Denotes the maximum number of iterations in which the Pareto set can remain unchanged before termination.
  #   itMax - (Integer) If specified, algorithm will terminate after this iteration
  #   nTweak - (Integer) maximum number of candidate solutions generated during an iteration
  #   print_it_results - (logical) Indicates whether to print individual iteration results to the console
  #   total_servers - (Numeric) Decision variable upper bound - useful for allocation problems
  #   warmup - (Numeric) How long before the DES reaches steady state
  #   sim_length - (Numeric) How long should the DES record statistics after the warmup period
  #   obj_function_list - (Character) Vector of function names used as objective functions 
  #   optim_type - (Character) Vector indicating if the corresponding objective function is maximized ("max" or "+") or minimized ("min" or "-")
  #   nVar - (Integer) Number of decision variables
  #   init_sol - (Numeric) Starting solution vector.
  #   generate_plots - (Logical) Indicates whether to produce plots of the Pareto image as it evolves
  
  # Function Return (List):
  #   sched_type - (Character) hyper parameter indicating how the temperature changes over time. 
  #   t_damp: (Numeric) temperature modification parameter, effect of this parameter depends on the chosen cooling schedule
  #   nTweak - (Integer) maximum number of candidate solutions generated during an iteration
  #   total_iterations - (Integer) Number of DB-PSA iterations used
  #   nReplications - (Integer) Total number of simulation replications used
  #   pareto_set - (List) List of solutions belonging to the estimated Pareto set. Each object contains a name, solution vector, objective function means, standard deviation and confidence interval
  #   itRepsDF - (Data frame) Shows the number of replications used in each iteration
  #   History - (List) History of the algorithm performance. Holds the pareto set at each iteration and all rejected candidate solutions
  #   execution_time - (Numeric) Time taken to complete the DB-PSA algorithm
  
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
  
  # Set the cooling schedule from function inputs and other temperature related params
  temp <- temp_init
  t_min <- .01 * temp_init
  cool_env <- environment()
  reduce_temp  <-
    function()
      cool_temp(cool_sched = sched_type, .envir = cool_env)
  
  # Initialize algorithm performance variables/parameters
  stat_logical <- T
  maxChange <- 1
  itReps_Cum  <- 0
  nTotal_Cum <- 0
  delta <- max(ceiling(nTweak / 2), 10)
  A <- list()
  i <- it <- 1
  optim_type <- fix_optim_type(optim_type)

  if(!continue_previous){
    now <- Sys.time()
    if(!use_test_bench){
      siteInfo <- (...)
    }
    # Generate Initial Baseline Solution ------------------------------------------------------------------------------
    initial <- list(
      name = 'Baseline',
      Solution = init_sol,
      Replications = 12,
      Allocation = decode(init_sol),
      counter = 0,
      Dist = 0,
      deltaPsi = 0
    )

    best <- initial <-
      updateSimStats(i = initial,
                     data = objective_Metrics(x = CostFunction(sol = initial$Allocation,
                                                               logic = F)),
                     new_sol = T)
    
    
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
  while (termination_criteria()) {
    
    extraReps <- F # Conditional for if extra simulation replications were used in the MOCBA
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
        pareto_set <- reorder_pSet(pSet = pareto_set,.envir = parent.frame())
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
      if (!is.na(results_directory) & !hyper) {
        save.image(file = file.path(results_directory, 'paused_envr.rdata'))
      }
      it %+% 1
      
      # Adjust Temperature ----------------------------------------------------------------------------------------------
      if (!identical(pareto_set, prev_pareto_set)) {
        temp <- reduce_temp()
      }
    } else {
      break
    }
  }
  exec_time <- as.numeric(Sys.time() - now)
    
    res_list <- list(
      'sched_type' = sched_type,
      't_damp' = t_damp,
      'nTweak' = nTweak,
      'total_iterations' = it,
      'nReplications' = itReps_Cum,
      'pSet' = pareto_set,
      'itReps' = itRepsDF,
      'execution_time' = exec_time,
      'history' = A)
    
    if (generate_plots) {
      lapply(
        X = seq_along(A),
        FUN = function(ind) {
          jpeg(file = file.path(plot_dir,
                                paste0(
                                  'Iteration_', it, '_pareto_image.jpeg'
                                )))
          plotParetoFront(A[[ind]]$itBest)
          dev.off()
        }
      )
    }
    
    if (hyper) {
      saveRDS(res_list, file = file.path(results_directory, paste0(
        paste(sched_type, t_damp, nTweak, sep = '_'), '.rds'
      )))
    }
    return(res_list)
}