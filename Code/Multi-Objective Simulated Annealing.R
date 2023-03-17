# Algorithm Set Up --------------------------------------------------------
rm(list = ls())
use_test_bench <- T
inverted_V_logical <- T
continue_previous <- F

# Load necessary files ----------------------------------------------------
source(file.path('.','Code','functions.R'))
source(file.path('.','Code','MOSA Functions.R'))

if(!continue_previous) {
  if (use_test_bench) {
    source(file = file.path('.','Code','Test_Bed_Optimization.r'))
  } else {
    source(file = file.path('.','Code','Full_Sim_Optimization.r'))
  }
  
  # Initialize First Solution and Algorithm Hyper-parameters  ---------------------------------------------------------------------------
  temp_init <- temp <- 3
  t_min <- .01 * temp_init
  t_damp <-
   #0.01 #Quad Cool alpha
   0.75 #Exponetial Cool alpha
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
    Replications = 20, # Should == reps argument from CostFunction function (MOSA Functions.R line 150)
    Allocation = decode(init_sol),
    counter = 0,
    Dist = 0,
    deltaPsi = 0
  )
  
  init_data <-
    objective_Metrics(
      data = CostFunction(
        sol = initial$Allocation,
        logic = F,
        test_bench = use_test_bench,
        use_inv_V = inverted_V_logical
      ),
      fun_list = obj_function_list
    )
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
  while (termination_criteria()) {
    browser()
    now <- Sys.time()
    extraReps <- F # Conditional for if extra simulation replications were used in the MOCBA
    # Generate Candidates for the Iteration ---------------------------------------------------------------------------
    temp_obj <- gen_candidates(tweak_left = nTweak, s_star = best)
    if (length(temp_obj) != 0) {
      # Perform OCBA to Minimize Total Simulation Replications ----------------------------------------------------------
      temp_obj <- ocba(candidate_list = temp_obj)
      ocba_removed <- temp_obj[['non_sP']]
      ocba_removed <- ocba_removed[which(lapply(ocba_removed,length) !=0)]
      temp_obj <- temp_obj[['sP']]
      
      # Find the Fittest Candidate --------------------------------------------------------------------------------------
      if (it >= 1) {
        prev_pareto_set <- pareto_set
      }
      pareto_set <-
        updateParetoSet(pSet = pareto_set, candidate_list =  temp_obj)
       
      ranks <- pareto_set$ranks
      pareto_set <- pareto_set$pSet
      pareto_order <- data.table(name = pareto_set %c% 'name',t(pareto_set %c% 'Obj_mean'))[,.(apply(.SD,2,unlist))][,order(mean_server_utilization,max_mean_queue,avg_wait,decreasing = F)]
      pareto_set <- pareto_set[pareto_order]
      
      prev_best <- best
      best <- findBestbyDistance(pSet = pareto_set)
      
      if (identical(prev_best$name, best$name)) {
        best_counter <- best_counter + 1
      } else {
        best_counter <- 0
      }
      A <-
        updateHistory(pSet = pareto_set,
                      candidate_list = append(temp_obj,ocba_removed),
                      history = A)
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
        if (compareIterationFronts(pareto_set, prev_pareto_set)) {
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
      all_allocations <- rbind(all_allocations,t(temp_obj %c% 'Allocation'))
      # Tabu Style removal of earlier tested solutions ---------------------------------------------------------------------------------------------- # nolint
      #if (it > (pareto_limit - 1)) {
        #all_allocations <-
        #  all_allocations[-seq(length(A[[it - (pareto_limit - 1)]]$Rejects)), ]
        #tested_allocs <-
        #  rbind(tested_allocs, t(temp_obj %c% 'Allocation'))
      #} else {
       # tested_allocs <- all_allocations
      #}
      if (it %% 1 == 0) {
        cat(
          'Iteration',
          it,
          'required',
          itReps,
          'simulation replications.')
          cat('\n')
          cat('The temperature is now',
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
          cat(
            'An extra',itReps - candidate_reps,
            'replications were used during MOCBA \n'
          )
        }
        cat('\n')
        if(length(pareto_set) > 1){
          print(
            rbindlist(lapply(pareto_set, function(i)
              data.table(
                name = i$name,
                p_selection = i$P_Selection,
                divergence = i$Divergence,
                i$Obj_CI
              )))
          )
        }
        cat('\n')
      }
      save.image(file = file.path(res_dir, 'paused_envr.rdata'))
      it %+% 1
      
      # Adjust Temperature ----------------------------------------------------------------------------------------------
      if(!identical(pareto_set,prev_pareto_set)){
      temp <-
        cool_temp(
          initial_temperature = temp_init,
          alpha = t_damp,
          current_iteration = it,
          exponential = T
        )
        }
    } else {
      break
    }
  }
  
  save.image(file = file.path(
    res_dir,
    "Inverted_V_Full_Environments",
    paste0("Trial_", i, ".Rdata")
  ))
  
  if(i == 1){
    instance_df <- best_df[,instance := i]
  } else {
    instance_df <- rbind(instance_df,best_df[,instance := i])
  }

pareto_objectives <- 
  t(matrix(as.matrix(pareto_set %c% 'Obj_mean'),ncol = length(pareto_set)))

saveRDS(
  object = list(
    instance_df,
    pareto_set,
    plotParetoFront(inputData = pareto_set)
  ),
  file = file.path(res_dir, 'Algorithm Trial Information, Pareto Fronts and Plots.rds')
)
tested_allocs <-
  unique(x = apply(
    X = tested_allocs,
    MARGIN = 1,
    FUN = paste,
    collapse = ','
  ))

tested_allocs <- matrix(as.numeric(unlist(sapply(tested_allocs,strsplit,split = ','))),ncol = 4,byrow = T)