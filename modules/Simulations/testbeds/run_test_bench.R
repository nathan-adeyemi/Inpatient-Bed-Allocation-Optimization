obj_name <- function(x) {
  deparse(substitute(x))
}

run_test_bench <-
  function(rep_nums = NA,
           network_df,
           multicore = T,
           sim_length = NA,
           warmup = NA,
           inverted_V = T) {
    
    n_queues <- network_df[,.N]
    arrival_gen <- function(envr, id, inv_V = T) {
      if(!inv_V){
        id <- as.character(id)
        eval(parse(
          text = paste0(
            obj_name(envr),
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
          obj_name(envr), " %>% add_resource(paste('queue_",id,"'),
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
                        simmer::release(resource = 'queue_",id,"',amount = 1) %>%", 
                          ifelse(test = inverted_V, 
                                 yes = " join(queueing_node_leave_trajectory)", 
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