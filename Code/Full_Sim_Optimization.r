suppressMessages(source('.Rprofile'))
# continue_previous <-
#   readline(prompt = 'Continue previous DB-PSA algorithm run? (y/n)')
# continue_previous <-
#   grep(pattern = 'y|yes',
#        x = continue_previous,
#        ignore.case = T)
continue_previous <- F
res_dir <-
  file.path(".", "Data", "Full Sim Results", gsub(pattern = '-',replacement = '_',x = Sys.Date()))

source(file.path('Simulations', 'Minnesota MH Network Simulation.R'))
siteInfo <-
  data.table(readRDS(
    file.path('Simulations', 'Function Requirements', 'Rates5.rds')
  ))
obj_function_list <- c("mh_distance_range","mh_wait_quantile","mh_wait_sigma")
warmup <- 30
sim_length <- 200

if (continue_previous) {
  res_dir <-
    file.path(res_dir, paste('Trial', length(list.files(res_dir)), sep = '_'))
  n_iter <-
    max(na.omit(unique(unlist(
      lapply(X = strsplit(list.files(res_dir), '_'), FUN = as.numeric)
    ))))
  history <-
    readRDS(file.path(res_dir, paste(
      'Iteration', n_iter, 'history.rds', sep = '_'
    )))
  pSet <- history[[length(history)]]$itBest
  
  results <- DB_PSA(
    continue_previous = T,
    results_directory = res_dir,
    sched_type = 'q',
    t_damp = 0.45,
    nTweak = 7,
    initial_trials = 18,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = c("min", "min", "min"),
    nVar = length(siteInfo[, unique(Bed_Group)]),
    init_sol = unlist(sapply(
      X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
      FUN = function(i)
        c(1, rep(x = 0, times = i - 1))
    )),
    use_test_bench = F,
    total_servers = total_servers,
    generate_plots = T,
    print_it_results = T,
    hyper = F,
    siteInfo = siteInfo,
    pareto_set = pSet,
    A = history,
    env_path = file.path(res_dir, paste('Iteration', n_iter, 'paused_envr.rdata', sep = '_'))
  )
} else {
  if (!dir.exists(res_dir)) {
    dir.create(res_dir)
    res_dir <-
      file.path(res_dir, 'Trial_1')
    dir.create(res_dir)
  } else {
    res_dir <-
      file.path(res_dir, paste('Trial', length(list.files(res_dir)) + 1, sep = '_'))
  }
  
  results <- DB_PSA(
    continue_previous = F,
    results_directory = res_dir,
    sched_type = 'q',
    t_damp = 0.45,
    nTweak = 7,
    initial_trials = 18,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = c("min", "min", "min"),
    nVar = length(siteInfo[, unique(Bed_Group)]),
    init_sol = unlist(sapply(
      X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
      FUN = function(i)
        c(1, rep(x = 0, times = i - 1))
    )),
    use_test_bench = F,
    total_servers = total_servers,
    generate_plots = T,
    print_it_results = T,
    hyper = F,
    siteInfo = siteInfo
  )
}
