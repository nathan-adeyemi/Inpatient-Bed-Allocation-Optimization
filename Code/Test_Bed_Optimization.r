rm(list = ls())
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
source(file.path('.', 'Code', 'functions.R'))
source(file.path('.', 'Code', 'MOSA Functions.R'))
source(file.path('.', 'Code', 'Multi-Objective Simulated Annealing.R'))

inverted_V_logical <- T
use_test_bench <- T
continue_previous <- F
size <- 'Medium'

# Directory to Store MOSA Results -----------------------------------------
res_dir <- file.path(".", "Data", "Sample MOSA Results", gsub("-", "_", Sys.Date()))
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}
res_dir <- file.path(res_dir, paste0("Trial_", length(list.files(res_dir)) + 1))
dir.create(res_dir)
read_init <- T
jackson_envir <- new.env()
if (read_init) {
  starter_data <-
    readRDS(file.path("Data", paste0(size," Testing Initial Solution.rds")))
  queues_df <- starter_data$network_df
  n_queues <- nVar <- queues_df[, .N]
} else {
  print(queues_df)
}
sys.source(file.path(".", "Code", "Jackson Network Test Bench.R"), envir = jackson_envir)

obj_function_list <-
  grep(
    pattern = "TB_",
    x = lsf.str(),
    value = T
  )
init_sol <- c(1, rep(0, (nVar - 1)))
sim_length <- 1000
warmup <- 100
# Run Optimization Algorithm------------------------------------------------
results <- DB_PSA(
  results_directory = res_dir,
  sim_length = sim_length,
  nTweak = 7,
  warmup = warmup,
  obj_function_list = obj_function_list,
  optim_type = c("max", "min"), #c('max','min','min')
  nVar = queues_df[, .N],
  use_test_bench = use_test_bench,
  total_servers = total_servers,
  generate_plots = F
)
