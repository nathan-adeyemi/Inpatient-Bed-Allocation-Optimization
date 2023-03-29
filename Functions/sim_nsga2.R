sim_nsga2 <- function(inp_vec, .envir = parent.frame()) {
  if (!all(inp_vec %% 1 == 0)) {
    inp_vec <- decode(inp_vec)
  }
  res <-
    run_test_bench(
      rep_nums = seq(10),
      network_df = .envir$queues_df[, server_count := inp_vec],
      multicore = F,
      sim_length = .envir$sim_length,
      warmup = .envir$warmup,
      inverted_V = .envir$inverted_V_logical
    )
  res <- apply(
    X = objective_Metrics_nsga2(
      res,
      fun_list = .envir$obj_function_list,
      value = T
    )[,-1],
    MARGIN = 2,
    FUN = mean
  )
  return(res)
}
