mh_distance_range <- function(x){
  return(x[type == 'Transfer',.(variable = diff(range(Travel.Distance,na.rm = T))),by = list(Age,replication)
  ][,.(max_travel_range = max(variable)),by = replication])
}