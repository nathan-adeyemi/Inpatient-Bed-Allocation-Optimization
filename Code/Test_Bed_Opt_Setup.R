inverted_V_logical <- T
use_test_bench <- T
stat_logical <- T

args <- commandArgs(trailingOnly=TRUE)
if(length(args) > 0){
  size <- as.character(args[1])
  obj_function_list <- unlist(strsplit(args[2],","))
  optim_type <- unlist(strsplit(args[3],","))
} else {
  size <- 'Large'
  optim_type <- c('max', 'min')
  obj_function_list <- c('TB_obj_1', 'TB_obj_2')

}
read_init <- T
jackson_envir <- new.env()

if (read_init) {
  starter_data <-
    readRDS(file.path(
      ".",
      "Data",
      'Test Inverted V Networks',
      paste0(size, " Testing Initial Solution.rds")
    ))
  queues_df <- starter_data$network_df
  n_queues <- queues_df[, .N]
}

sys.source(file.path(".", "Code", "Jackson Network Test Bench.R"), envir = jackson_envir)
n_obj <- length(obj_function_list)
optim_type_print <- optim_type
obj_fun_print <- obj_function_list
obj_fun_print[n_obj] <- paste0("and ",obj_fun_print[n_obj])
optim_type_print[n_obj] <- paste0("and ",optim_type_print[n_obj])

cat("Test Network size is:", size)
cat("\n")
cat("Objective metrics are", paste0(obj_fun_print,collapse = `if`(n_obj == 2," ",", ")))
cat("\n")
cat('Optimization directions are',paste0(optim_type_print,collapse = `if`(n_obj == 2," ",", ")))
cat("\n")

init_sol <- c(1, rep(0, (n_queues - 1)))
if(grepl(pattern = 'Small',
         x = size,
         ignore.case = T)) {
  sim_length <- 2000
  warmup <- 150
} else if (grepl(pattern = 'Medium',
                 x = size,
                 ignore.case = T)) {
  sim_length <- 3000
  warmup <- 200
} else{
  sim_length <- 5000
  warmup <- 500
}
