calc_steady_state_metrics <- function(df){
  jackson_network <-
    # err <- cat("ERROR :", conditionMessage(e))
    quiet(queueing::QueueingModel(x = queueing::NewInput2.OJN(
      prob = p_ijs[seq(n_queues), seq(n_queues)],
      nodes = lapply(
        X = seq(n_queues),
        FUN = function(row) {
          queueing::NewInput.MMC(
            lambda = df[row, lambda_cap],
            mu = df[row, service_rates],
            c = df[row, server_count],
            n = 0,
            method = 0
          )
        }
      )
    )))
  
  # df <- df[, `:=`(
  #   p_0 = queueing::Pn(jackson_network),
  #   N_j_analytical = queueing::Lk(jackson_network),
  #   W_q_analytical = queueing::Wk(jackson_network),
  #   rho_analytical = queueing::ROk(jackson_network)
  # )]
  ret_list <- c(rho_analytical = mean(queueing::ROk(jackson_network)),
                L_q_analytical = max(queueing::Lk(jackson_network)),
                W_q_analytical = queueing::W(jackson_network))
  return(ret_list)
}