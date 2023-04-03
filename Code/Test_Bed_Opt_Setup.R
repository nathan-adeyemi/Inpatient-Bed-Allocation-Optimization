inverted_V_logical <- T
continue_previous <- F
use_test_bench <- T
# bi_objective <- F

size <- 'Medium'
read_init <- T
jackson_envir <- new.env()

if (read_init) {
  starter_data <-
    readRDS(file.path(
      "Data",
      'Test Inverted V Networks',
      paste0(size, " Testing Initial Solution.rds")
    ))
  queues_df <- starter_data$network_df
  n_queues <- queues_df[, .N]
}

sys.source(file.path(".", "Code", "Jackson Network Test Bench.R"), envir = jackson_envir)
if (bi_objective) {
  optim_type <- c('max', 'min')
  obj_function_list <- c('TB_obj_1', 'TB_obj_2')
} else {
  optim_type <- c("max", "min", "min")
  obj_function_list <- c('TB_obj_1', 'TB_obj_2', 'TB_obj_3')
}

init_sol <- c(1, rep(0, (n_queues - 1)))
sim_length <- 3000
warmup <- 200