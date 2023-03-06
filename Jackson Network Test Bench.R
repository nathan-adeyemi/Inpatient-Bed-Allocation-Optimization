# DES Functions (build trajectories and generators, run simulation, process ) from parameters -----------------------------------------------------------------------------------------
obj.name <- function(x) {
  deparse(substitute(x))
}

calc_metrics <- function(envrs){
  arrivals_df <- data.table(get_mon_arrivals(envrs)
  )[start_time > warmup,
    ][dcast(data = data.table(get_mon_attributes(envrs)
    )[, .(value = max(value)), by = list(name, key, replication)],
    formula = name + replication ~ key,
    value.var = 'value'),
    n_jobs_completed := n_jobs_completed, 
    on = c(name = 'name', replication = 'replication')]
  
  
  res_df <-
    data.table(get_mon_resources(data)
    )[, `:=`(usage = server / capacity)
      ][time > warmup,
        ][, `:=`(diff = system - data.table::shift(system, n = 1, 
                                                   type = 'lag')),by = list(resource)]
  
  class(res_df) <- c('resources','data.table','data.frame')
  
  mean_sojourn_sim <-
    arrivals_df[,`:=`(sojourn_time = end_time - start_time,
                      origin_queue = extract_queue(name))
                ][,.(mean_sojourn_time = mean(sojourn_time,na.rm = T)),by = list(origin_queue,replication)
                  ][,.(mean_sojourn_time = list(signif(t.test(mean_sojourn_time, conf.level  = .95)$conf.int, 4))),by = origin_queue]
  
  mean_N_j_sim <-
    res_df[,.(N_j = mean(system)),by = list(resource,replication)
           ][,(N_j = list(signif(t.test(N_j, conf.level  = .95)$conf.int, 4))),by = resource]
  
  return(list(arrivals = arrivals_df,
              resources = res_df,
              mean_sojourn_sim = mean_sojourn_sim,
              mean_N_j_sim = mean_N_j_sim))
}

run_test_bench <-
  function(rep_nums = NA,
           network_df,
           multicore = T,
           sim_length = NA,
           warmup = NA,
           inverted_V = T) {
    
    arrival_gen <- function(envr, id, inv_V = T) {
      if(!inv_V){
        id <- as.character(id)
        eval(parse(
          text = paste0(
            obj.name(envr),
            "%>% add_generator(
              name_prefix = 'arrivals_queue_",id,"_',
              trajectory = queueing_node_",id,"_init,
              distribution = from_to(
                 start_time =0,
                 stop_time = sim.length + warmup + 1,
                 dist = function()
                    rexp(n = 1, rate = network_df[",id,", lambda_cap]),
                 arrive = FALSE
              ),
              mon = 2)"
          )
        ))
      }else{
        envr %>% 
          add_generator(
            name_prefix = 'central_arrivals',
            trajectory = central_queue,
            distribution = from_to(
              start_time =0,
              stop_time = sim.length + warmup + 1,
              dist = function()
                rexp(n = 1, rate = sum(network_df[, lambda_cap])),
              arrive = FALSE
            ),
            mon = 2)
      }
      return(envr)
    }
    
    probabilistic_routing <- function(.environ, inv_V = inverted_V) {
      queue_params <-  if(inv_V) list(seq(n_queues),network_df[,p_ijs]) else list(seq(n_queues + 1), p_ijs[get_attribute(.env =  get('environ_name', pos = -1) , keys = 'current_queue'),])
      return(sample(
        x = queue_params[[1]],
        size = 1,
        replace = T,
        prob = queue_params[[2]]
      ))
    }
    move_environ_name <- function(.env,target_envir){
      assign(x = 'environ_name',value = .env,envir = target_envir)
      return(.env)
    }
    
    queue_server_gen <- function(envr, id) {
      eval(parse(
        text = paste0(
          obj.name(envr), " %>% add_resource(paste('queue_",id,"'),
           capacity = network_df[",id,",server_count], queue_size = Inf)"
        )
      ))
      return(envr)
    }
    
    rep_nums <- ifelse(test = is.na(rep_nums),
                       yes = 1,
                       no = rep_nums)
    
    sim.length <-
      ifelse(test = is.na(sim_length),
             yes = 500,
             no = sim_length)
    
    warmup <- 
      ifelse(test = is.na(warmup),
             yes = 50,
             no = warmup)
    
    extract_queue <- Vectorize(function(name) paste0('queue_',suppressWarnings(as.numeric(na.omit(as.numeric(unlist(strsplit(name,"_"))))[1]))))
    
    traj_list <- list() #initiate trajectory list so to avoid an error when creating the branch_trajectory
    
    branch_trajectory <- trajectory('branch_traj')
    
    queueing_node_leave_trajectory <- trajectory('exit_trajectory') %>% leave(prob = 1)
    
    # Queueing Node Trajectories (Post Routing) -------------------------
    for (id in seq(n_queues)){
      eval(parse(text =
                   paste0("queueing_node_",id,"_trajectory <- trajectory(name = 'queue_",id,"_traj',verbose = F) %>%
                        simmer::seize(resource = 'queue_",id,"',amount = 1) %>%
                        simmer::set_attribute(keys = 'current_queue',values = ",id,") %>% 
                        simmer::set_attribute(keys = 'n_jobs_completed',values = 1,mod = '+',init = 0) %>%
                        simmer::timeout(
                           task = function()
                              rexp(n = 1, rate = network_df[",id,", service_rates])) %>%
                        simmer::release(resource = 'queue_",id,"',amount = 1)  %>%", 
                          ifelse(test = inverted_V, 
                                 yes = "join(queueing_node_leave_trajectory)", 
                                 no = "rollback(amount = 6, times = Inf)")
                          
                   )
      )
      )
    }
    
    traj_list <- mget(intersect(grep('queueing_node_',ls(),value = T),grep('trajectory',ls(),value = T)))
    
    # Routing Trajectory ------------------------------------------------------
    branch_trajectory <- trajectory('branch_traj') %>%
      branch(function() probabilistic_routing(),
             continue = F,
             traj_list)
    
    # Queueing Node Trajectories (New Network Arrivals) -------------------------------------------------
    if(!inverted_V){
      for (id in seq(n_queues)){
        eval(parse(text =
                     paste0("queueing_node_",id,"_init <- trajectory(name = 'queue_",id,"_traj',verbose = F) %>%
                          simmer::seize(resource = 'queue_",id,"',amount = 1) %>%
                          simmer::set_attribute(keys = 'n_jobs_completed',values = 1,mod = '+',init = 0) %>%
                          simmer::set_attribute(keys = 'current_queue',values = ",id,") %>%
                          simmer::timeout(
                             task = function()
                                rexp(n = 1, rate = network_df[",id,", service_rates])) %>%
                          simmer::release(resource = 'queue_",id,"',amount = 1) %>%
                          simmer::join(branch_trajectory)"
                     )
        )
        )
      }
    }else{
      central_queue <- trajectory(name = 'central', verbose = F) %>%
        simmer::join(branch_trajectory)
    }
    sim_env <-  environment()
    
    if (multicore) { #any(Sys.info()['sysname'] == c('Darwin', 'Linux'))
      data <- mclapply(
        X = rep_nums,
        FUN = function(i)
          simmer() %>%
          move_environ_name(target_envir = sim_env) %>%
          queue_server_gen(seq(n_queues)) %>%
          arrival_gen(seq(n_queues),inv_V = inverted_V) %>%
          simmer::run(until = sim.length + warmup) %>%
          simmer::wrap(),
        mc.cores = detectCores() - 1,
        mc.set.seed = T
      )
    } else {
      data <- foreach(n = rep_nums) %do% {
        function(i)
          simmer() %>%
          move_environ_name(target_envir = sim_env) %>%
          queue_server_gen(seq(n_queues)) %>%
          arrival_gen(seq(n_queues),inv_V = inverted_V) %>%
          simmer::run(until = sim.length + warmup)
      }()
      #stopCluster(cl)
      reap_zombies()
    }
    return(list(
      setDT(get_mon_arrivals(data,ongoing = T))[start_time >= warmup,
                                                ][is.na(end_time),`:=`(end_time = sim.length,
                                                                       activity_time = 0)],
      setDT(get_mon_resources(data))[time >= warmup,
                                     ][,utilization := 100 * (server/capacity)]
    ))
  }

full_test_bench <- function(queues_df, reps, multicore = TRUE) {
  data <- run_sim(queues_df,replications = reps,parallel = multicore)
  test <- calc_metrics(data)
  return(queues_df[test$resources[,.(avg = mean(usage)),by = list(resource,replication)
                                  ][,.(avg = mean(avg)),by = resource
                                    ][,resource := as.numeric(gsub(pattern = 'queue_',replacement = '',x = resource))],
                   rho_sim := avg,on = c(queue_id = 'resource')
                   ][test$resources[diff == 1,
                                    ][,timediff := time - data.table::shift(time,n = 1,type = 'lag'),by = list(replication,resource)
                                      ][,.(avg = mean(timediff,na.rm = T)),by = list(replication,resource)
                                        ][,.(avg = 1/mean(avg)),by = resource][,resource := as.numeric(gsub(pattern = 'queue_',replacement = '',x = resource))],
                     lambda_sub_sim := avg,on = c(queue_id = 'resource')
                     ][test$resources[,.(avg = mean(usage)),by = list(resource,replication)
                                      ][,.(avg = mean(avg)),by = resource
                                        ][,resource := as.numeric(gsub(pattern = 'queue_',replacement = '',x = resource))],
                       rho_sim := avg,on = c(queue_id = 'resource')])
}

# Objective Functions for Test Bench MOSA ---------------------------------
TB_obj_1 <- function(x) {
  if (is.list(x)) {
    x = x[[2]]
  }
  z <- x[, .(server_util = mean(utilization, na.rm = T)), by = list(resource,replication)
         ][is.na(server_util),server_util := 0
           ][,.(mean_server_utilization = weighted.mean(server_util,na.rm = T)), by = replication]
  return(z)
}

TB_obj_2 <- function(x) {
  if (is.list(x)) {
    z <-
      x[[2]][, .(queue_length = mean(system)), by = list(resource, replication)][, .(max_mean_queue = max(queue_length)), by = replication]
    # return(x[[2]][,mean_server_utilization <- mean(utilization,na.rm = T), by  = replication]
  } else{
    z <-
      x[, .(time_std_dev = sd(activity_time, na.rm = T)), by = list(replication)]
  }
  return(z)
}

TB_obj_3 <- function(x){
  #browser(expr = get('it',envir = .GlobalEnv))
  return(
    x[[1]][, wait_time := Vectorize(function(x, y, z) { max(0, ((x - y) - z))})(end_time, start_time, activity_time)
           ][,.(avg_wait = mean(wait_time,na.rm = T)),by = replication])
}

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

# Jackson Network QT Steady State Analysis ------------------
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
# Functions for Analytical QT Calculations  -------------------------------
p_k_calc <- Vectorize(function(j,k){
  if (k <= queues_df[j,server_count]) {
    return(queues_df[j,p_0] * (queues_df[j,rho] ^ k) / factorial(k))
  } else {
    return(queues_df[j,p_0] * (queues_df[j,rho] ^ k) /
             (factorial(queues_df[j,server_count]) *
                queues_df[j,server_count] ^ (k - queues_df[j,server_count])))
  }
})

# Generate Random Network of Queues, Arrival Rates, etc. ----------------------------------------------------------

# Minimum and maximum values for network parameters (number of servers, queues, distance)
# n_queues <- 25
# min_server <- 1
# max_server <- 5
# max_dist <- 10
# min_arr_rate <- 0
# max_arr_rate  <- 0.5
# service_excess_factor <-  1.01 #How much should the network service rate exceed the network arrival rate

# Transition probabilities and intra-node distances
#d_ijs <- as.matrix(rnddist(n_queues)) * max_dist
if(identical(environment(),.GlobalEnv)){
  inverted_V_logical <-
    readline(prompt = 'What type of queueing Network? \n 1. Inverted-V \n 2. Fully Connected Open Jackson \n 3. Closed Jackson Network')
 if(inverted_V_logical == 1){
   inverted_V_logical <- T
 }else{
   inverted_V_logical <- F
 }
  n_queues <- 5
}
min_server <- 0
max_server <- 5
max_dist <- 10
min_arr_rate <- 0.1
max_arr_rate  <- 1
service_excess_factor <- 1.01 #How much should the network service rate exceed the network arrival rate
total_servers <- 4 * n_queues

if(!get('read_init',envir = .GlobalEnv)){
if(!inverted_V_logical){
# Define probabilities of routing from one queue to another in a completely interconnected Jackson network ----------------------------------------------------------
p_ijs <- matrix(data = runif((n_queues + 1)^2), nrow = n_queues + 1, ncol = n_queues + 1)
p_ijs <- t(apply(
  X = p_ijs,
  MARGIN = 2,
  FUN = function(i)
    i / sum(i)
))

# Generating random values for individual queues for fully connected Open Jackson Network ----------------------------------------------------------
queues_df <- data.table(queue_id = seq(n_queues),
                        lambda_cap = runif(n = n_queues,
                                           min = min_arr_rate,
                                           max = max_arr_rate))
lambda_sub <-
  solve(a = diag(n_queues) - t(p_ijs[seq(n_queues), seq(n_queues)]),
        b = queues_df[,lambda_cap])

queues_df <- queues_df[,`:=`(lambda_sub = lambda_sub)
                       ][,service_rates := runif(n = .SD[,.N],
                                                 min = 0,
                                                 max = 1)]
} else {
  # Define routing probabilities in a inverted-V queueing network --------------------------------------------------------
  p_ijs <-
    matrix(
      data = runif(n = n_queues, min = 0, max = 1) %>% 
        {function(x) x/sum(x)}(),
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
  queues_df[, `:=`(server_count = rep(NA, n_queues)), ]
}

# ][, service_rates := runif(
#   n = 1,
#   min = lambda_sub / server_count,
#   max = ceiling(lambda_sub / server_count)
# ), by = queue_id
# ][,`:=`(rho = lambda_sub/service_rates)]
  list2env(
    list(
      'total_servers' = total_servers,
      'run_test_bench' = run_test_bench,
      'queues_df' = queues_df,
      'TB_obj_1' = TB_obj_1,
      'TB_obj_2' = TB_obj_2,
      'TB_obj_3' = TB_obj_3
    ),
    .GlobalEnv
  )
} else{
  list2env( list(
    'total_servers' = total_servers,
    'run_test_bench' = run_test_bench,
    'TB_obj_1' = TB_obj_1,
    'TB_obj_2' = TB_obj_2
    #'TB_obj_3' = TB_obj_3
  ),.GlobalEnv)
}