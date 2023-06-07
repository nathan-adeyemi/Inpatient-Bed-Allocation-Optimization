mh_total_throughput <- function(x){
  baseline_throughput <- 12745 #Baseline's simulation's average total patients treated over the simulation window
  return(x[!is.na(Finish.Timestamp)][,.N,by = replication][,(.total_throughput = 100 * ((N - baseline_throughput)/baseline_throughput)),by = replication])
}