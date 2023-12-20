suppressMessages(source('.Rprofile'))


source(file.path('Simulations', 'Minnesota MH Network Simulation.R'))
siteInfo <-
  data.table(readRDS(
    file.path('Simulations', 'Function Requirements', 'Rates5.rds')
  ))
orig_alloc_path <- file.path('.','Data','plotting_utilities','Baseline Simulation Results','Patients Results (All Replications).rds')
  
args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0) {
  obj_function_list <- unlist(strsplit(args[1], ","))
  optim_type <- unlist(strsplit(args[2], ","))
  continue_previous <-
    grepl(pattern = "T|TRUE",
          x = as.character(args[3]),
          ignore.case = T)
} else {
  continue_previous <-
    readline(prompt = 'Continue previous DB-PSA algorithm run? (y/n)')
  continue_previous <-
    grepl(pattern = 'y|yes',
         x = continue_previous,
         ignore.case = T)
  obj_function_list <-
    c("mh_distance_range", "mh_wait_quantile", "mh_wait_sigma")
  optim_type <- rep("min", 3)
}
warmup <- 30
sim_length <- 200

n_obj <- length(obj_function_list)
optim_type_print <- optim_type
obj_fun_print <- obj_function_list
obj_fun_print[n_obj] <- paste0("and ", obj_fun_print[n_obj])
optim_type_print[n_obj] <- paste0("and ", optim_type_print[n_obj])

cat("Objective metrics are", paste0(obj_fun_print, collapse = `if`(n_obj == 2, " ", ", ")))
cat("\n")
cat('Optimization directions are',
    paste0(optim_type_print, collapse = `if`(n_obj == 2, " ", ", ")))
cat("\n")

if(n_obj == 4) {
  full_sim_folder <- "Full Sim Results (4 Objectives)"
} else if (any(grepl("mh_total_throughput", obj_function_list))) {
  full_sim_folder <- "Full Sim Results (Throughput Objective)"
} else{
  full_sim_folder <- "Full Sim Results"
}

res_dir <-
  file.path(".",
            "Data", full_sim_folder)
if (continue_previous) {
  
  res_dir <-
    file.path(res_dir, paste('Trial', max(na.omit(unique(
      unlist(lapply(
        X = strsplit(list.files(res_dir, pattern = 'Trial_'), '_'), FUN = as.numeric
      ))
    ))), sep = '_'))
  
  cat('The results file path is:', res_dir)
  n_iter <-
    max(na.omit(unique(unlist(
      lapply(X = strsplit(list.files(
        file.path(res_dir, 'Iteration Environments')
      ), '_'), FUN = as.numeric)
    ))))
  
  results <- DD_PUSA(
    continue_previous = T,
    results_directory = res_dir,
    sched_type = 'q',
    temp_init = 100,
    t_damp = 0.45,
    nTweak = 7,
    initial_trials = 18,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = optim_type,
    nVar = length(siteInfo[, unique(Bed_Group)]),
    init_sol = unlist(sapply(
      X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
      FUN = function(i)
        c(1, rep(x = 0, times = i - 1))
    )),
    use_test_bench = F,
    generate_plots = T,
    print_it_results = T,
    pareto_limit = 20,
    siteInfo = siteInfo,
    env_path = file.path(
      res_dir,
      'Iteration Environments',
      paste('Iteration', n_iter, 'paused_envr.rdata', sep = '_')
    )
  )
} else {
  res_dir <-
    file.path(res_dir, paste('Trial', length(list.files(res_dir,pattern = 'Trial_')) + 1, sep = '_'))
  dir.create(res_dir)
  results <- DD_PUSA(
    continue_previous = F,
    results_directory = res_dir,
    temp_init = 100,
    sched_type = 'q',
    t_damp = 0.45,
    nTweak = 7,
    initial_trials = 18,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = optim_type,
    nVar = length(siteInfo[, unique(Bed_Group)]),
    init_sol = unlist(sapply(
      X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
      FUN = function(i)
        c(1, rep(x = 0, times = i - 1))
    )),
    use_test_bench = F,
    pareto_limit = 20,
    generate_plots = T,
    print_it_results = T,
    hyper = F,
    siteInfo = siteInfo,
    orig_alloc_path = file.path(
      "~",
      "MH_Simulation",
      "Policy Interventions to Improve Mental Healthcare Access",
      "Data",
      "Simulation and Alternatives Results",
      "Validation Results",
      "temp_results_11_14",
      "Trial_3",
      "Patients Results (All Replications).rds"
    )
  )
}
solutions_df_congfig_2 <- 
  rbindlist(lapply(
  X = pSet_config_2,
  FUN = function(soln)
    with(soln, cbind(
      data.table(t(
        with(soln,
             apply(
               X = rbind(Obj_mean[, lapply(X = .SD, FUN =  round, digits = 2)], 
                         Obj_CI[, lapply(X = .SD,FUN = extractHalfWidth)
                                ][, lapply(X = .SD, FUN = round, digits = 2)]),
               MARGIN = 2,
               FUN = paste,
               collapse = ' +/- '
             ))), 
    bedShiftFn(sol = Allocation,counts_by_age = T),
    `Total Beds Moved` = bedShiftFn(sol = Allocation,total_displaced = T)
    ))
)))

solutions_df_config_2 <- 
  rbindlist(lapply(
    X = pSet_config_2,
    FUN = function(soln)
      with(soln, cbind(
        data.table(t(
          with(soln,Obj_mean[, lapply(X = .SD, FUN =  round, digits = 2)], 
          bedShiftFn(sol = Allocation,counts_by_age = T),
          `Total Beds Moved` = bedShiftFn(sol = Allocation,total_displaced = T)
        )))
      ))))
