source('.Rprofile')
res_dir <-
  file.path(".", "Data", "Full Sim Results", gsub('-', '_', Sys.Date()))
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}
res_dir <-
  file.path(res_dir, paste0('Trial_', length(list.files(res_dir)) + 1))
dir.create(res_dir)
unfinished_dir <-file.path(res_dir, paste0('Trial_', length(list.files(res_dir)) + 1))

if (dir.exists(unfinished_dir)) {
  load(unfinished_dir)
  continue_previous <- T
} else{
  # Directory to Store MOSA Results -----------------------------------------
  
  source(file.path('Simulations', 'Minnesota MH Network Simulation.R'))
  siteInfo <-
    data.table(readRDS(
      file.path('Simulations', 'Function Requirements', 'Rates5.rds')
    ))
  obj_function_list <-
    grep(pattern = 'mh_',
         x = lsf.str(),
         value = T)
  init_sol <- 
  warmup <- 30
  sim_length <-  200
  continue_previous <- F
}
results <- DB_PSA(
  results_directory = res_dir,
  sched_type = 'q',
  t_damp = 0.45,
  nTweak = 12,
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
  generate_plots = F,
  print_it_results = T,
  hyper = F,
  siteInfo = siteInfo
)