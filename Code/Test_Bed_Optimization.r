inverted_V_logical <- T
use_test_bench <- T
continue_previous <- F
# Directory to Store MOSA Results -----------------------------------------
res_dir <- file.path(".", "Data","Sample MOSA Results",gsub('-','_',Sys.Date()))
if(!dir.exists(res_dir)){
  dir.create(res_dir)
}
res_dir <- file.path(res_dir,paste0('Trial_',length(list.files(res_dir))+1))
dir.create(res_dir)
read_init <- T
n_queues <- nVar <- 1
jackson_envir <- new.env()
optim_type <- c('max', 'min','min')
if (read_init) {
  starter_data <-
    readRDS(file.path('Data', 'Medium Testing Initial Solution (4 queues).rds'))
  queues_df <- starter_data$network_df
  n_queues <- nVar <- queues_df[,.N]
}
sys.source(file.path('.','Code','Jackson Network Test Bench.R'),envir = jackson_envir)
print(queues_df)
obj_function_list <- 
  grep(pattern = 'TB_',
        x = lsf.str(),
        value = T)
init_sol <- c(1, rep(0, (nVar - 1)))
sim_length <- 500
warmup <- 25
# Run Optimization Algorithm------------------------------------------------
source(file.path('.','Code','Multi-Objective Simulated Annealing.R'))