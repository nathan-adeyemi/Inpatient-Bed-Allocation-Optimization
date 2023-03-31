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