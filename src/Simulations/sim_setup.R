rm(list = ls())
invisible(source(file.path('src','Simulations','.Rprofile')))

args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
  port_val <- args[[1]]
  client_socket <- make.socket(host = 'localhost', port = as.numeric(port_val), server = F, fail = T)
  network_size <- args[[2]]
  if ((length(args)) == 3){
    obj_fns <- eval(parse(text=args[[3]]))
  }
}

if(grepl(network_size,pattern = 'ed_to_ip')){
  warmup <- 30
  sim_length <- 200
  run_sim <- function(sol) MH.Network.sim(rep = 1, warm = warmup, sim_days = sim_length, alg_input = sol, n.parallel = 1, resources = FALSE)

  if(!exists('obj_fns')){
    obj_fns <- grep('mh_',ls(),value = T)
  }

} else {

  if(!exists('obj_fns')){
    obj_fns <- grep('TB_',ls(),value = T)
  }

  data_dir <- file.path(
      "src",
      "Simulations",
      "testbeds",
      "sim_data")

  matching_files <- list.files(path = data_dir, pattern = paste0(network_size, " Testing Initial Solution.rds"),ignore.case = TRUE)

  starter_data <- readRDS(file.path(data_dir,matching_files[1]))

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
  run_sim <- function(sol) run_test_bench(rep_nums = 1, network_df = queues_df[,server_count := sol])
} 

# Nonstop loop to evaluate any allocation
while (T){
  allocation <- read.socket(socket = client_socket)
  allocation <- gsub(pattern = '\\[|\\]',replacement  = '',allocation) |> strsplit(split = ',') |> unlist() |> as.numeric()
  res <- run_sim(allocation)
  res = objective_Metrics(x = res, obj_function_list = obj_fns)
  res = res[,replication := NULL]
  write.socket(socket = client_socket, string = jsonlite::toJSON(x=res))
}



