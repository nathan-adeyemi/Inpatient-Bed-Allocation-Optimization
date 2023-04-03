rm(list = ls())
source('.Rprofile')
bi_objective <- T
source(file.path("~","MH_Simulation","Inpatient Bed Allocation Optimization","Code","Test_Bed_Opt_Setup.R"))

# Directory to Store MOSA Results -----------------------------------------
res_dir <- file.path(".", "Data", "Sample MOSA Results", gsub("-", "_", Sys.Date()))
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}
res_dir <- file.path(res_dir, paste0("Trial_", length(list.files(res_dir)) + 1))
dir.create(res_dir)

# Run Optimization Algorithm------------------------------------------------
results <- DB_PSA(
  results_directory = res_dir,
  nTweak = 7,
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
