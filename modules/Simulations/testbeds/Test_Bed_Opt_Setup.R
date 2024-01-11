rm(list = ls())
invisible(source('.Rprofile'))
inverted_V_logical <- T
use_test_bench <- T
stat_logical <- T

args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
  port_val <- args[[1]]
  network_size <- args[[2]]
}

# Read the connection port supplied by the python routing agent.
if(!exists('port_val')){
  port_val <- readline("Insert the port number:")
}
network_size <- `if`(exists('network_size'),network_size,'Small')
client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)

optim_type <- c('max', 'min')
obj_function_list <- c('TB_obj_1', 'TB_obj_2')

starter_data <-
  readRDS(file.path(
    "modules",
    "Simulations",
    "testbeds",
    "sim_data",
    paste0(network_size, " Testing Initial Solution.rds")
  ))
queues_df <- starter_data$network_df
n_queues <- queues_df[, .N]
total_servers <- 4 * n_queues

if(grepl(pattern = 'Small',
         x = network_size,
         ignore.case = T)) {
  sim_length <- 2000
  warmup <- 200
} else if (grepl(pattern = 'Medium',
                 x = network_size,
                 ignore.case = T)) {
  sim_length <- 3000
  warmup <- 200
} else{
  sim_length <- 5000
  warmup <- 500
}

# Evenly split total servers among each server pool (test scenario)
queues_df[,server_count := as.numeric(server_count)
          ][,server_count := as.numeric(total_servers/nrow(queues_df))]
# Begin running the simulation when the test allocation is sent
while (T){
  allocation <- read.socket(socket = client_socket)
  allocation <- gsub(pattern = '\\[|\\]',replacement  = '',allocation) |> strsplit(split = ',') |> unlist() |> as.numeric()
  test <- run_test_bench(rep_nums = 1, network_df = queues_df[,server_count := allocation])
  test = objective_Metrics(x = test, obj_function_list = c('TB_obj_1','TB_obj_2'))
  test = test[,replication := NULL]
  write.socket(socket = client_socket, string = jsonlite::toJSON(x=test))
}