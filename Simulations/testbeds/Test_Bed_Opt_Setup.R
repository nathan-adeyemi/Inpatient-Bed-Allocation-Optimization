rm(list = ls())
source('.Rprofile')
inverted_V_logical <- T
use_test_bench <- T
stat_logical <- T

# Read the connection port supplied by the python routing agent.
if(!exists('port_val')){
  port_val <- readline("Insert the port number:")
}
client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)
network_size <- read.socket(socket = client_socket)

args <- commandArgs(trailingOnly=TRUE)

network_size <- `if`(exists('network_size'),network_size,'Small')
optim_type <- c('max', 'min')
obj_function_list <- c('TB_obj_1', 'TB_obj_2')
jackson_envir <- new.env()


starter_data <-
  readRDS(file.path(
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
  sim_length <- 200
  warmup <- 15
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

# Transmit the test network size to the RL agent
write.socket(socket = client_socket, string = as.character(queues_df[, .N]))

# Begin running the simulation when the test allocation is sent
allocation <- read.socket(socket = client_socket)
allocation <- gsub(pattern = '\\[|\\]',replacement  = '',allocation) |> strsplit(split = ',') |> unlist() |> as.numeric()
test <- run_test_bench(rep_nums = 1, network_df = queues_df)
test = objective_Metrics(x = test, obj_function_list = c('TB_obj_1','TB_obj_2'))
test = test[,replication := NULL]
write.socket(socket = client_socket, string = jsonlite::toJSON(x=test))