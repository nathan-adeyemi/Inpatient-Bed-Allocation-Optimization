rm(list = ls())
source('.Rprofile')
bi_objective <- F
continue_previous <- readline(prompt = 'Continue previous DB-PSA algorithm run? (y/n)')
continue_previous <-
  grepl(pattern = 'y|yes',
       x = continue_previous,
       ignore.case = T)
source(file.path("~","MH_Simulation","Inpatient Bed Allocation Optimization","Code","Test_Bed_Opt_Setup.R"))

# Directory to Store MOSA Results -----------------------------------------
res_dir <- file.path(".", "Data", "Sample MOSA Results", gsub("-", "_", Sys.Date()))
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}
if(!continue_previous){
  res_dir <- file.path(res_dir, paste0("Trial_", length(list.files(res_dir)) + 1))
  dir.create(res_dir)
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
                    print_it_results = T)
} else{
  res_dir <- file.path(res_dir, paste0("Trial_", length(list.files(res_dir))))
  n_iter <- max(na.omit(unique(unlist(lapply(strsplit(list.files(res_dir),'_'),as.numeric)))))
  load(file.path(res_dir,paste('Iteration', n_iter,'paused_envr.rdata',sep = '_')))
  history <- readRDS(file.path(res_dir,paste('Iteration', n_iter,'history.rds',sep = '_')))
  pSet <- history[[length(history)]]$itBest
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
  print_it_results = T,
  pareto_set = pSet,
  A = history
  )
