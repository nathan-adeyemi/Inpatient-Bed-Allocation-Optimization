CostFunction <- function(sol = NULL,
                         logic,
<<<<<<< HEAD
                         reps = NA,
                         # nsga = F,
                         .envir = parent.frame()) {
  
  # Runs the simulation (via the simmer package) with the new solution vector
  
  # Inputs:
  #   sol: (Numeric) A solution vector
  #   reps: (Integer) The number of simulation replications to be executed
  #   logic: (Logical) should simulation replications be run on multiple cores
  
  # Returns:
  #   x: (List) AS list of simmer output data.frames
  
  
  # if (nsga) {
  #   sol <- decode(sol)
  # }
  if(is.na(reps)){
    reps <- seq(.envir$initial_trials)
  } else if (all(length(reps) == 1,reps != 1)){
    reps <- seq(reps)
  }
  
  if (.envir$use_test_bench == T) {
=======
                         reps = seq(10),
                         analytical = FALSE,
                         nsga = F,
                         .envir = parent.frame()) {
  if (nsga) {
    sol <- decode(sol)
  }
  
  if (analytical == T) {
    
  } else if (.envir$use_test_bench == T) {
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
    x <- run_test_bench(
      rep_nums = reps,
      network_df = copy(queues_df)[, server_count := sol],
      multicore = logic,
      sim_length = .envir$sim_length,
      warmup = .envir$warmup,
      inverted_V = .envir$inverted_V_logical
    )
  } else{
<<<<<<< HEAD
    x <-  MH.Network.sim(
      rep = reps,
      warm = .envir$warmup,
      sim_days = .envir$sim_length,
      sort_by_prob = F,
      n.parallel = 1,
      alg_input = sol,
      resources = F
    )
  }
  # if (nsga) {
  #   x <- apply(X = objective_Metrics_nsga2(x, fun_list = .envir$obj_function_list)[,-1],
  #              MARGIN = 2,
  #              FUN = mean)
  # }
=======
    x <-
      full_sim(
        num_iter = reps,
        parallel = logic,
        new_sol = sol,
        warmup = .envir$warmup,
        sim_length = .envir$sim_length,
        save_files = F,
        return_resources = F
      )
  }
  if (nsga) {
    x <- apply(X = objective_Metrics_nsga2(x, fun_list = grep('TB_', lsf.str(envir = .envir), value = T))[,-1],
               MARGIN = 2,
               FUN = mean)
  }
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
  return(x)
}