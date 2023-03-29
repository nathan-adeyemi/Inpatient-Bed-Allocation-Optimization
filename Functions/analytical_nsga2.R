analytical_nsga2 <- function(inp_vec,.envir = parent.frame()){
  if(!all(inp_vec %% 1 == 0)){
    inp_vec <- decode(inp_vec)
  }
  tryCatch({
    res <-
      calc_steady_state_metrics(queues_df[, server_count := inp_vec])
    return(res * c(-1, 1, 1))
  },
  error = function(e) {
    # err <- cat("ERROR :", conditionMessage(e))
    return (c(
      N_j_analytical = Inf,
      W_q_analytical = Inf,
      rho_analytical = Inf
    ))
  })
}
