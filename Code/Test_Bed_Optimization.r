<<<<<<< HEAD
<<<<<<< HEAD
rm(list = ls())
source('.Rprofile')
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
# continue_previous <-
#   readline(prompt = 'Continue previous DB-PSA algorithm run? (y/n)')
# continue_previous <-
#   grepl(pattern = 'y|yes',
#         x = continue_previous,
#         ignore.case = T)
continue_previous <- F
>>>>>>> 2d8d6de (Further Updates)
source(
  file.path(
    "~",
    "MH_Simulation",
    "Inpatient Bed Allocation Optimization",
    "Code",
    "Test_Bed_Opt_Setup.R"
  )
)
<<<<<<< HEAD

# Directory to Store MOSA Results -----------------------------------------
res_dir <-
  file.path(".", "Data", paste0("Testbench Results (",length(optim_type)," Objectives)"),size)
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}
  # res_dir <-
  #   file.path(res_dir, paste0("Trial_", length(list.files(res_dir)) + 1))
  #dir.create(res_dir)
for(instance in seq(15)){
  results <- DD_PUSA(
    continue_previous = F,
    results_directory = res_dir,
    nTweak = 7,
    sched_type = 'q',
    temp_init = 100,
    t_damp = 0.45,
    sim_length = sim_length,
    initial_trials = 8,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = optim_type,
    nVar = n_queues,
    use_test_bench = use_test_bench,
    total_servers = total_servers,
    generate_plots = T,
    print_it_results = T,
    tabu_limit = `if`(size == 'Small',7,12)
    )
  
  results <-
    c(
      results,
      percent_correct = pareto_perc_correct(
        i = results$pSet,
        size = size,
        extras = F
      ),
      extra_solns = pareto_perc_correct(
        i = results$pSet,
        size = size,
        extras = T
      )
    )
  if (!dir.exists(file.path(res_dir, 'Instance Results'))) {
    dir.create(file.path(res_dir, 'Instance Results'))
  }
  saveRDS(results, file = file.path(
    res_dir,
    'Instance Results',
    paste0('Instance_', instance, '_results.rds')
  ))
  par.env <- environment()
  if(instance == 1){
    instance_df <- with(results,
                        data.table(perc_correct = percent_correct,
                              extra_solns = extra_solns,
                              exec_time = execution_time,
                              total_replications = nReplications,
                              nIterations = total_iterations,
                              g_ideal1 = apply(find_g_ideal(pSet = pSet, .envir = par.env),2,mean)[1],
                              g_ideal_2 = apply(find_g_ideal(pSet = pSet, .envir = par.env),2,mean)[2]))
  } else{
    instance_df <- rbind(instance_df,
                         with(results,
                              data.table(perc_correct = percent_correct,
                                         extra_solns = extra_solns,
                                         exec_time = execution_time,
                                         total_replications = nReplications,
                                         nIterations = total_iterations,
                                         g_ideal1 = apply(find_g_ideal(pSet = pSet, .envir = par.env),2,mean)[1],
                                         g_ideal_2 = apply(find_g_ideal(pSet = pSet, .envir = par.env),2,mean)[2])))
  }
}
  # saveRDS(results, file.path(res_dir, paste0(size, '_Network_DD_PUSA_results.rds')))
  saveRDS(instance_df, file.path(res_dir, paste0(size, '_Network_DD_PUSA_dataframe.rds')))
  
=======
=======
rm(list = ls())
<<<<<<< HEAD
source(paste0(
  '/',
  file.path(
    'home',
    'adeyemi.n',
    'MH_Simulation',
    'Inpatient Bed Allocation Optimization',
    '.Rprofile'
  )
))

>>>>>>> 1487b4d (Commit Attempt 2)
inverted_V_logical <- T
use_test_bench <- T
continue_previous <- F
size <- 'Medium'
=======
source('.Rprofile')
=======
bi_objective <- T
>>>>>>> d020c10 (Updated function descriptions for some of the DB_PSA functions.)
=======
bi_objective <- F
continue_previous <- readline(prompt = 'Continue previous DB-PSA algorithm run? (y/n)')
continue_previous <-
  grepl(pattern = 'y|yes',
       x = continue_previous,
       ignore.case = T)
>>>>>>> f4d354d (Further Updates:)
source(file.path("~","MH_Simulation","Inpatient Bed Allocation Optimization","Code","Test_Bed_Opt_Setup.R"))
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
=======
>>>>>>> 2d8d6de (Further Updates)

# Directory to Store MOSA Results -----------------------------------------
res_dir <-
  file.path(".", "Data", "Sample MOSA Results", gsub("-", "_", Sys.Date()))
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}
if (!continue_previous) {
  res_dir <-
    file.path(res_dir, paste0("Trial_", length(list.files(res_dir)) + 1))
  dir.create(res_dir)
  results <- DB_PSA(
    continue_previous = continue_previous,
    results_directory = res_dir,
    nTweak = 5,
    sched_type = 'q',
    t_damp = 0.4,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = optim_type,
    nVar = n_queues,
    use_test_bench = use_test_bench,
    total_servers = total_servers,
    generate_plots = T,
    print_it_results = T
  )
  saveRDS(results,file.path(res_dir,'DB_PSA_results.rds'))
} else{
  res_dir <-
    file.path(res_dir, paste0("Trial_", length(list.files(res_dir))))
  n_iter <-
    max(na.omit(unique(unlist(
      lapply(strsplit(list.files(res_dir), '_'), as.numeric)
    ))))
  load(file.path(
    res_dir,
    paste('Iteration', n_iter, 'paused_envr.rdata', sep = '_')
  ))
  history <-
    readRDS(file.path(res_dir, paste(
      'Iteration', n_iter, 'history.rds', sep = '_'
    )))
  pSet <- history[[length(history)]]$itBest
<<<<<<< HEAD
  results <- DB_PSA(continue_previous = continue_previous,
                    results_directory = res_dir,
                    nTweak = 5,
                    sched_type = 'q',
                    t_damp = 0.4,
                    sim_length = sim_length,
                    warmup = warmup,
                    obj_function_list = obj_function_list,
                    optim_type = optim_type,
                    nVar = n_queues,
                    use_test_bench = use_test_bench,
                    total_servers = total_servers,
                    generate_plots = T,
                    print_it_results = T,
                    pareto_set = pSet,
                    A = history
  )
  }

<<<<<<< HEAD
obj_function_list <-
  grep(
    pattern = "TB_",
    x = lsf.str(),
    value = T
  )
init_sol <- c(1, rep(0, (nVar - 1)))
<<<<<<< HEAD
<<<<<<< HEAD
sim_length <- 2500
warmup <- 200
init_sol <- c(1, rep(0, (nVar - 1)))
<<<<<<< HEAD
>>>>>>> e29de9b (Updates:)
=======

=======
sim_length <- 500
warmup <- 25
>>>>>>> db7855f (Issue: calls for "objective_metrics" function cannot read from global environment)
# Run Optimization Algorithm------------------------------------------------
source(file.path('.','Code','Multi-Objective Simulated Annealing.R'))
>>>>>>> bc28031 (Calls optimization algorithm directly from Test_Bed/Full_Sim Optimization scripts)
=======
sim_length <- 1000
warmup <- 100
=======
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
# Run Optimization Algorithm------------------------------------------------
results <- DB_PSA(continue_previous = T,
  results_directory = res_dir,
  nTweak = 5,
  sched_type = 'q',
  t_damp = 0.4,
  sim_length = sim_length,
  warmup = warmup,
  obj_function_list = obj_function_list,
  optim_type = optim_type,
  nVar = n_queues,
  use_test_bench = use_test_bench,
  total_servers = total_servers,
  generate_plots = T,
<<<<<<< HEAD
  print_it_results = T
)
>>>>>>> 1487b4d (Commit Attempt 2)
=======
  print_it_results = T,
  pareto_set = pSet,
  A = history
  )
>>>>>>> f4d354d (Further Updates:)
=======
  results <- DB_PSA(
    continue_previous = continue_previous,
    results_directory = res_dir,
    nTweak = 5,
    sched_type = 'q',
    t_damp = 0.4,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = optim_type,
    nVar = n_queues,
    use_test_bench = use_test_bench,
    total_servers = total_servers,
    generate_plots = T,
    print_it_results = T,
    pareto_set = pSet,
    A = history
  )
  saveRDS(results,file.path(res_dir,'DB_PSA_results.rds'))
}
>>>>>>> 2d8d6de (Further Updates)
