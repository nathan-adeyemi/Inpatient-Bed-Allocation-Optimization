# Algorithm Set Up --------------------------------------------------------
rm(list = ls())

# Directory to Store MOSA Results -----------------------------------------
res_dir <- file.path(".","Data","Sample MOSA Results",gsub('-','_',Sys.Date()))
if(!dir.exists(res_dir)){
  dir.create(res_dir)
}
res_dir <- file.path(res_dir,paste0('Trial_',length(list.files(res_dir))+1))
dir.create(res_dir)

setwd("/home/adeyemi.n/MH_Simulation/Inpatient Bed Allocation Optimization")
source(file.path('functions.R'))
source('MOSA Functions.R')
use_test_bench <-  T
inverted_V_logical <- T
# Prompt for if a previous optimization problem needs to be comple --------
# continue_previous <-
#   readline(prompt = 'Continue DB-PSA from previously saved environment? (Y/N):')
# continue_previous <-
#   grepl(pattern = 'y|yes|True|T',
#         x = continue_previous,
#         ignore.case = T)
continue_previous <- F

if(!continue_previous) {
  if (use_test_bench) {
    # read_init <-
    #   readline(prompt = 'Read from an initial solution? (Y/N):')
    # read_init <- grepl(pattern = 'y|yes|True|T',
    #                    x = read_init,
    #                    ignore.case = T)
    read_init <- T
    n_queues <- nVar <- 1
    jackson_envir <- new.env()
    optim_type <- c('max', 'min','min')
    if (read_init) {
      starter_data <-
        readRDS(file.path('Data', 'Medium Testing Initial Solution (4 queues).rds'))
      queues_df <- starter_data$network_df
      n_queues <- nVar <- queues_df[,.N]
    }
    sys.source('Jackson Network Test Bench.R',envir = jackson_envir)
    print(queues_df)
    obj_function_list <- 
      grep(pattern = 'TB_',
           x = lsf.str(),
           value = T)
    init_sol <- c(1, rep(0, (nVar - 1)))
    sim_length <- 2500
    warmup <- 200
  } else {
    source(file.path('Simulations', 'Minnesota MH Network Simulation.R'))
    siteInfo <-
      data.table(readRDS(
        file.path('Simulations', 'Function Requirements', 'Rates5.rds')
      ))
    obj_function_list <-
      grep(pattern = 'mh_',
           x = lsf.str(),
           value = T)
    nVar <- length(siteInfo[,unique(Bed_Group)])
    optim_type = rep('min', 3)
    init_sol <- unlist(sapply(
      X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
      FUN = function(i)
        c(1, rep(x = 0, times = i - 1))))
      warmup <- 1
      sim_length <-  10
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
  nTweak <- 8
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
  init_sol <-
    sl_ifelse(test = use_test_bench,
              yes = c(1, rep(0, (nVar - 1))),
              no = )
  #init_sol <- runif(n = n_queues,min = 0,max = 1)
  initial <- list(
    name = 'Baseline',
    Solution = init_sol,
    Replications = 20, # Should == reps argument from CostFunction function (MOSA Functions.R line 150)
    Allocation = if (!use_test_bench) siteInfo[!duplicated(Bed_Group), total_beds] else decode(alg_input = init_sol),
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
} else {
  if (!use_test_bench) {
    load(file = file.path('Data', 'full_sim_paused_envr.rdata'))
  } else{
    load(file = file.path('Data', 'test_bench_paused_envr.rdata'))
  }
}
  # Main Optimization Algorithm Loop ---------------------------------------------------------------------------------
  while (termination_criteria()) {
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
        # print(pareto_set %c% 'Obj_CI')
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
      browser()
      if (!use_test_bench) {
        save.image(file = file.path('Data', 'full_sim_paused_envr.rdata'))
      } else{
        save.image(file = file.path(res_dir, 'test_bench_paused_envr.rdata'))
      }
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
  # best_df_long <- melt(data = copy(best_df)[,`:=`(Dist = NULL, 
  #                                                 Num_Replications = NULL)],
  #                      measure.vars = colnames(best$Obj_mean))
  
  print(paste('Algorithm Trial',i,'Complete'))
  save.image(file = file.path(
    res_dir,
    "Inverted_V_Full_Environments",
    paste0("Trial_", i, ".Rdata")
  ))
  
  if(i == 1){
    instance_df <- best_df[,instance := i]
    # best_allocations <- data.table(t(best$Allocation))
  } else {
    instance_df <- rbind(instance_df,best_df[,instance := i])
    # best_allocations <- rbind(best_allocations,t(best$Allocation))
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

# Run Same Problem w/ NSGA-II Algorithm -----------------------------------
# if(use_test_bench) {
if(FALSE){
  nsgaCostFn <- function(x) {
    return(CostFunction(sol = x,
                 logic = F,
                 nsga = T))
  }
  test_nsga2 <-
    mcnsga2(
      fn = nsgaCostFn,
      varNo = nVar,
      objDim = length(optim_type),
      lowerBounds = rep(0, nVar),
      upperBounds = rep(, nVar),
      popSize = 40,
      generations = 100,
      cprob = 0.7,
      mprob = 0.2
    )
  
  saveRDS(test_nsga2,
          file = file.path(res_dir, 'NSGA_II_results.rds'))
  idx <-
    apply(
      X = unique(t(apply(test_nsga2$parameters,1,decode,test_bench = T))),
      MARGIN = 1,
      FUN = function(a) apply(t(apply(test_nsga2$parameters,1,decode,test_bench = T)), 1, function(b) all(a == b))
    )
  idx <- which(!duplicated(apply(idx,1,which)))
  nsga_sols <- test_nsga2$parameters[idx,]
  nsga_pareto_front <- lapply(seq_along(idx),function(x) update_sol(i = x,sol_vector = nsga_sols[x,]))
  nsga_pareto_front <- gen_candidates(candidate_list = nsga_pareto_front)
  
  saveRDS(
    list(
      `full_results` = test_nsga2,
      `pareto_front_plot` = plotParetoFront(inputData = nsga_pareto_front),
      `nsga_pareto_front` = nsga_pareto_front
    ),
    file = file.path(res_dir, 'NSGA_II_results.rds')
  )
}
