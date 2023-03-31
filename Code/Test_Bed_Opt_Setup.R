inverted_V_logical <- T
continue_previous <- F
use_test_bench <- T

size <- 'Small'
read_init <- T
jackson_envir <- new.env()
optim_type <- c("max", "min", "min")

if (read_init) {
  starter_data <-
    readRDS(file.path(
      "Data",
      'Test Inverted V Networks',
      paste0(size, " Testing Initial Solution.rds")
    ))
  queues_df <- starter_data$network_df
  n_queues <- nVar <- queues_df[, .N]
}

sys.source(file.path(".", "Code", "Jackson Network Test Bench.R"), envir = jackson_envir)
obj_function_list <-
  grep(pattern = "TB_",
       x = lsf.str(),
       value = T)
init_sol <- c(1, rep(0, (nVar - 1)))
sim_length <- 2500
warmup <- 150