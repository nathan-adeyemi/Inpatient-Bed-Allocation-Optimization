objective_Metrics <- function(x, obj_function_list){
  
  # Applies the selected objective functions to the simulation data
  
  # Inputs: 
  #   x: (List) A list of Simmer output data.frames
  
  # Returns:
  #   data: (Data.table) A data.table of the calculated objective metrics for each simulation replication 
  
  if(is.list(x)){
    if(grepl("ed_ip_sim", cfg_name)){
      x[[1]] <- x[[1]][Enter.Timestamp > (24 * warmup)]
    } else {
      x[[1]] <- x[[1]][start_time >  warmup]  
    }
    x[[2]] <- x[[2]][time > warmup]
  } else{
    if(grepl("ed_ip_sim", cfg_name)){
      x <- x[Enter.Timestamp > (24 * warmup)]
    } else {
      x <- x[start_time > warmup]  
    }
  }
  data <- lapply(
    X = obj_function_list,
    FUN = function(func) {
      do.call(what = func, args = list(x))
    }
  )
    data <- Reduce(merge,data)
    return(data)
}


# Read in function information from python master script --------------------
if(!interactive()){
  port_val <- get_sys_env_var(var_name = 'port', var_class = 'numeric')
  cfg_name <- get_sys_env_var(var_name = 'cfg_name', var_class = 'character')
  cfg_path <- get_sys_env_var(var_name = 'cfg_path', var_class = 'character')
  obj_fns <- get_sys_env_var(var_name = 'obj_fns', var_class = 'vector')
  solution <-get_sys_env_var(var_name = 'solution', var_class = 'vector')
  seed <- get_sys_env_var(var_name = 'seed', var_class = "integer")
  
} else{
  port_val <- NULL
  cfg_path <- "src/Simulations/sim_info.json"
  
  # cfg_name <- 'ed_ip_sim'
  # obj_fns <- c('mh_wait_quantile','mh_total_throughput')

  cfg_name <- 'testbed-small'
  obj_fns <- c('TB_obj_1','TB_obj_2')
  
  solution <- c(-6,-2,-2)
  seed <- NULL
}

cfg_name <-sub("/.*", "", cfg_name)
cfg <- jsonlite::read_json(path = cfg_path, simplifyVector = T)[[cfg_name]]


invisible(list2env(cfg, envir = environment()))

if (!is.null(port_val)) {
  client_socket <-
    make.socket(
      host = 'localhost',
      port = as.numeric(port_val),
      server = F,
      fail = T
    )
} else {
  client_socket <- NULL
}

args <- list(solution = solution)

# Define the Simulation funtion --------------------
if(grepl('ed_ip_sim',cfg_name)){

  args$acceptance_prob_input <- data.table(Facility_name = names(acceptance_probs), prob = unlist(acceptance_probs))
  args$ed_scale_param <- ed_scale_param
  
  sim_fn <- function(...) {
  MH.Network.sim(
    warm = warmup,
    sim_days = sim_length,
    resources = T,
    seed = seed,
    ... = ...
  )
  }
} else{
  sim_fn <- function(...){
    run_testbed_sim(
      rep_nums = 1, 
      network_df = queues_df, 
      sim_length = sim_length,
      warmup = warmup,
      inverted_V = TRUE,
      seed = seed,
      ...)
  }
}

if(interactive()){
  print("~~~~~~BEGINNING SIMULATION~~~~~~")
}

tryCatch(
  expr = {
    results = sim_fn(args)
    res = objective_Metrics(x = results, obj_function_list = obj_fns)[,replication := NULL]
    },
error = function(e){
   message("An error occurred: ", e$message)
})

if(interactive()){
  print(res)
}

if(!interactive()){
  transmit_results(results = res, receiver = client_socket)
} else {
print("~~~~~~~SIMULATION COMPLETE~~~~~~~")
}
