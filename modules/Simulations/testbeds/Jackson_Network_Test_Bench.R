# Transition probabilities and intra-node distances
# if (identical(environment(), .GlobalEnv)) {
#   inverted_V_logical <-
#     readline(prompt = 'What type of queueing Network? \n 1. Inverted-V \n 2. Fully Connected Open Jackson \n 3. Closed Jackson Network')
#   if (inverted_V_logical == 1) {
#     inverted_V_logical <- T
#   } else{
#     inverted_V_logical <- F
#   }
#   n_queues <- 5
# }
inverted_V_logical <- T
min_server <- 0
max_server <- 5
max_dist <- 10
min_arr_rate <- 0.1
max_arr_rate  <- 1
service_excess_factor <-
  1.01 #How much should the network service rate exceed the network arrival rate
total_servers <- 4 * n_queues

if (!get('read_init', envir = .GlobalEnv)) {
  if (!inverted_V_logical) {
    # Define probabilities of routing from one queue to another in a completely interconnected Jackson network ----------------------------------------------------------
    p_ijs <-
      matrix(data = runif((n_queues + 1) ^ 2),
             nrow = n_queues + 1,
             ncol = n_queues + 1)
    p_ijs <- t(apply(
      X = p_ijs,
      MARGIN = 2,
      FUN = function(i)
        i / sum(i)
    ))
    
    # Generating random values for individual queues for fully connected Open Jackson Network ----------------------------------------------------------
    queues_df <- data.table(
      queue_id = seq(n_queues),
      lambda_cap = runif(n = n_queues,
                         min = min_arr_rate,
                         max = max_arr_rate)
    )
    lambda_sub <-
      solve(a = diag(n_queues) - t(p_ijs[seq(n_queues), seq(n_queues)]),
            b = queues_df[, lambda_cap])
    
    queues_df <- queues_df[, `:=`(lambda_sub = lambda_sub)][, service_rates := runif(n = .SD[, .N],
                                                                                     min = 0,
                                                                                     max = 1)]
  } else {
    # Define routing probabilities in a inverted-V queueing network --------------------------------------------------------
    p_ijs <-
      matrix(
        data = runif(n = n_queues, min = 0, max = 1) %>%
          {
            function(x)
              x / sum(x)
          }(),
        nrow = 1,
        ncol = n_queues
      )
    
    # Generating random values for individual queues for inverted-V Network ----------------------------------------------------------
    
    lambda_total = sum(runif(n = n_queues,
                             min = min_arr_rate,
                             max = max_arr_rate))
    
    queues_df <- data.table(
      queue_id = seq(n_queues),
      lambda_cap = runif(n = n_queues,
                         min = min_arr_rate,
                         max = max_arr_rate),
      p_ijs = c(p_ijs)
    )[, lambda_sub := p_ijs * lambda_total][, `:=`(service_rates = runif(n = .SD[, .N],
                                                                         min = 0,
                                                                         max = 1))]
    
    
    min_t_servers <-
      queues_df[, .(sum(lambda_sub))] / (queues_df[, .(sum(service_rates))] * service_excess_factor ^ -1)
    queues_df[, `:=`(server_count = rep(NA, n_queues)),]
  }
}
total_servers <<- total_servers
