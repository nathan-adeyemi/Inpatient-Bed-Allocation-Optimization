# Algorithm Set Up --------------------------------------------------------
rm(list = ls())

# Directory to Store MOSA Results -----------------------------------------
res_dir <- file.path(".","Data","Sample MOSA Results",gsub('-','_',Sys.Date()))
if(!all(sapply(c(res_dir,file.path(res_dir,'Inverted_V_Full_Environments')),dir.exists))){
  dir.create(res_dir)
  dir.create(path = file.path(res_dir,
                              "Inverted_V_Full_Environments"))
}
setwd("..")
source(file.path('functions.R'))
setwd('Inpatient Bed Allocation Optimization')
source('MOSA Functions.R')

use_test_bench = T
inverted_V_logical = T
read_init <- T

if(use_test_bench) {
  if (read_init) {
    starter_data <-
      readRDS(file.path('Data', 'Small Testing Initial Solution.rds'))
    n_queues <- nrow(starter_data$network)
  } else{
    n_queues <- 3
  }
  source(file.path('Jackson Network Test Bench.R'))
  
}

# Initialize First Solution and Algorithm Hyper-parameters  ---------------------------------------------------------------------------
nVar <- ifelse(
  test = use_test_bench == TRUE,
  yes = n_queues,
  no = length(siteInfo[, unique(Bed_Group)])
)

if(use_test_bench){
  nVar = n_queues
  optim_type <- c('max','min','min')
} else {
  nVar <- siteInfo[unique(Bed_Group),.N]
  optim_type = rep('min',3)
}
temp_init <- 1.4
t_min <- 1.1 * temp_init
t_damp <- .001 #Depends on cooling schedule selection
best_limit <- 5
pareto_limit <- 25
starter_reps <- 5
stat_logical <- T
maxChange <- 0.25
itReps_Cum  <- 0
nTotal_Cum <- 0
sim_length <- 1500
warmup <- 100
nTweak <- 5
itMax <- 100
best_counter <- 0
delta <-  max(ceiling(nTweak/2),5)
A <- list()
mosa_trials <- 5

seed_val <- floor(runif(1)*10000) 
analytical <- F

for(i in seq(mosa_trials)) {
  temp <- temp_init
  it <- 0
  
  if(use_test_bench) {
    obj_function_list <-  
      grep(pattern = 'TB_',
           x = lsf.str(),
           value = T)
  } else {
    obj_function_list <-
      grep(pattern = 'mh_',
           x = lsf.str(),
           value = T)
  }
  num_obj <- length(obj_function_list)
  best_by_min_dist <- T
  
  # init_sol <- runif(nVar)
  if (!read_init){ 
    init_sol <- c(1,rep(0,(n_queues-1)))
  
  initial <- list(
    name = 'Baseline',
    Solution = init_sol,
    Replications = 20,
    # Should == reps argument from CostFunction function (MOSA Functions.R line 150)
    Allocation = decode(init_sol),
    counter = 0,
    Dist = 0,
    deltaPsi = 0
  )
  
  # Generate Initial Baseline Solution ------------------------------------------------------------------------------
  if (!use_test_bench) {
    initial$Allocation = siteInfo[!duplicated(siteInfo$Bed_Group), total_beds]
  } else {
    initial$Allocation <- decode(alg_input = initial$Solution)
  }
  
  init_data <-
    objective_Metrics(
      data = CostFunction(
        sol = initial$Allocation,
        logic = TRUE,
        test_bench = use_test_bench,
        use_inv_V = inverted_V_logical
      ),
      fun_list = obj_function_list
    ) 
  initial <- updateSimStats(i = initial, data = init_data, new_sol = T)
  best <- initial
  } else {
    starter_data$initial_sol
    best <- starter_data$initial_sol
    queues_df <- starter_data$network_df
  }
 
  # Initialize Dataframe of Best Solutions---------------------------------------------------------------------------
  best_df <-
    data.table(
      Iteration = 0,
      best$Obj_mean,
      Num_Replications = best$Cost[, max(replication)],
      Dist = 0
    )
  
  itRepsDF <-
    data.table(
      Iteration = 0,
      MOCBA = 0,
      Theoretical = 0,
      Temperature = temp_init
    )
  
  all_allocations <<- t(best$Allocation)
  pareto_set <-  list()
  
  # Main Optimization Algorithm Loop ---------------------------------------------------------------------------------
  while (termination_criteria(check_iteration = T)) {
    tempItDF <- data.table(Iteration = it, Time = Sys.time())
    if (it == 0) {
      itTimeDF = tempItDF
    } else {
      itTimeDF = rbind(itTimeDF, tempItDF)
    }
    
    # Generate Candidates for the Iteration ---------------------------------------------------------------------------
    
    temp_obj <- gen_candidates(nTweak)
    # if (length(temp_obj) == 0) {
    #   break
    # }
    
    # Perform OCBA to Minimize Total Simulation Replications ----------------------------------------------------------
    temp_obj <- ocba(candidate_list =  append(temp_obj, pareto_set))

    # Find the Fittest Candidate --------------------------------------------------------------------------------------
    if(it >= 1){
      prev_pareto_set <- pareto_set
    }
    pareto_set <-
      updateParetoSet(pSet = pareto_set, candidate_list =  temp_obj)
    
    ranks <- pareto_set$ranks
    pareto_set <- pareto_set$pSet
    
    prev_best <- best
    best <- findBestbyDistance(pSet = pareto_set)
    
    if(identical(prev_best$name,best$name)){
      best_counter <- best_counter + 1
    } else {
      best_counter <- 0
    }
    A <- updateHistory(pSet = pareto_set,candidate_list = temp_obj,history = A)
    
    # Kill Zombie Processes leftover from MOCBA
    if (!any(sapply(X = c('Linux', 'Darwin'), function(i)
      i == Sys.info()['sysname']))) {
      reap_zombies() # Kill Zombie processes
    }

    # Update Iteration Best and Iteration Info Dataframes -------------------------------------------------------------
    it %+% 1
    
    if (it == 1) {
      itReps_Cum <- itReps
      theoretical_cum <- N_Total
      pareto_counter <- 0
    } else{
      itReps_Cum %+% itReps
      theoretical_cum %+% N_Total
      if(compareIterationFronts(pareto_set,prev_pareto_set)){
        pareto_counter %+% 1
      } else {
        pareto_counter <- 0
      }
    }
    
    best_df <- rbindlist(list(
      best_df,
      data.table(
        Iteration = it,
        best$Obj_mean,
        Num_Replications = best$Replications,
        Dist = cramer::cramer.test(x = as.matrix(best$Cost)[, -1],
                                   y = matrix(data = c(rep(-100, times = 20), 
                                                       rep(0, times = 40)), nrow = 20)
                                   )$statistic
      )
    ))
    
    itRepsDF <-
      rbindlist(list(
        itRepsDF,
        data.table(
          Iteration = it,
          MOCBA = itReps_Cum,
          Theoretical = theoretical_cum,
          Temperature = temp
        )
      ))
    
    # Adjust Temperature ----------------------------------------------------------------------------------------------
    # temp <-
    #   cool_temp(
    #     initial_temperature = temp_init,
    #     alpha = t_damp,
    #     
    #     current_iteration = it,
    #     quad_cool = T
    #   )
    
    # Tabu Style removal of earlier tested solutions ----------------------------------------------------------------------------------------------
    if(it > (pareto_limit - 1)){
      all_allocations <-
        all_allocations[-length(A[[it -( pareto_limit - 1)]]$Rejects),]
    }
    if (it %% 1 == 0) {
      # print(pareto_set %c% 'Obj_CI')
      cat(
        'Iteration',
        it,
        'required',
        itReps,
        'simulation replications and',
        gsub(
          pattern = '_',
          replacement = " ",
          x = best$name
        ),
        'moves on.'
      )
      cat('\n')
      cat('The ideal point is', paste(g_ideal_CI, collapse = ', '), '.')
      cat('\n')
      }
  }
  best_df_long <- melt(data = copy(best_df)[,`:=`(Dist = NULL, 
                                                  Num_Replications = NULL)],
                       measure.vars = colnames(best$Obj_mean))
  
  
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
}

pareto_sets <- extractParetoSets(res_dir)
pareto_objectives <- pareto_objectives <- 
  t(matrix(as.matrix(pareto_sets %c% 'Obj_mean'),ncol = length(pareto_sets)))


instance_df_avgs <-
  setDT(
    melt(
      data = copy(instance_df)[, `:=`(
        mean_server_utilization = (100 * mean_server_utilization / .SD[Iteration == 0, mean_server_utilization]) -
          100,
        max_mean_queue = (100 * max_mean_queue / .SD[Iteration == 0, max_mean_queue]) - 100,
        avg_wait = (100 * avg_wait / .SD[Iteration == 0, avg_wait]) - 100 ,
        Dist = signif(Dist, digits = 4)
      ), by = instance], 
      measure.vars = colnames(best$Obj_mean)
    )
  )[,.(value = mean(value),sd = sd(value)),by = list(Iteration,variable)]

p <- ggplot(data = instance_df_avgs,
            mapping = aes(x = Iteration, y = value, colour = variable)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = value - sd,
                    ymax = value + sd),
                width = .2,
                position = position_dodge(0.05))
saveRDS(
  object = list(
    instance_df_avgs,
    p,
    instance_df,
    pareto_sets
  ),
  file = file.path(res_dir, 'Algorithm Trial Information, Pareto Fronts and Plots.rds')
)

# Run Same Problem w/ NSGA-II Algorithm -----------------------------------
if(use_test_bench) {
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
      upperBounds = rep(1, nVar),
      popSize = 30,
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
