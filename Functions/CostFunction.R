CostFunction <- function(sol = NULL,
                         logic,
                         reps = seq(10),
                         analytical = FALSE,
                         nsga = F,
                         .envir = parent.frame()) {
  if (nsga) {
    sol <- decode(sol)
  }
  
  if (analytical == T) {
    
  } else if (.envir$use_test_bench == T) {
    x <- run_test_bench(
      rep_nums = reps,
      network_df = copy(queues_df)[, server_count := sol],
      multicore = logic,
      sim_length = .envir$sim_length,
      warmup = .envir$warmup,
      inverted_V = .envir$inverted_V_logical
    )
  } else{
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
  return(x)
}