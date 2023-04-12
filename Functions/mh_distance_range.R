mh_distance_range <- function(x){
<<<<<<< HEAD
  return(x[type == 'Transfer',.(variable = diff(range(Travel.Distance,na.rm = T))),by = list(Age,replication)
=======
  return(x[,.(variable = diff(range(Travel.Distance,na.rm = T))),by = list(Age,replication)
>>>>>>> f4d354d (Further Updates:)
  ][,.(max_travel_range = max(variable)),by = replication])
}