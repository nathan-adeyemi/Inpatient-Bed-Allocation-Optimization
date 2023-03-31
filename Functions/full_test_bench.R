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