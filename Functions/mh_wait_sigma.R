mh_wait_sigma <- function(x){
  return(x[,.(variable = sd(total_wait_time,na.rm = T)^2),by = list(Age,replication)
  ][,.(wait_variance = max(variable)),by = replication])
}