mh_total_throughput <- function(x){
  baseline_throughput <- 12745 #Baseline's simulation's average total patients treated over the simulation window
<<<<<<< HEAD
<<<<<<< HEAD
  return(x[!is.na(Finish.Timestamp)][,.N,by = replication][,(.total_throughput = 100 * ((N - baseline_throughput)/baseline_throughput)),by = replication])
=======
  return(x[!is.na(Finish.Timestamp)][,.N,by = replication][,100 * ((N - baseline_throughput)/baseline_throughput),by = replication])
>>>>>>> f4d354d (Further Updates:)
=======
  return(x[!is.na(Finish.Timestamp)][,.N,by = replication][,(.total_throughput = 100 * ((N - baseline_throughput)/baseline_throughput)),by = replication])
>>>>>>> a420328 (Git Repo updates)
}