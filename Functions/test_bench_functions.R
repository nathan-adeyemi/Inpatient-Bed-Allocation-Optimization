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

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
TB_obj_2 <- function(x){
  #browser(expr = get('it',envir = .GlobalEnv))
  return(
    x[[1]][, wait_time := Vectorize(function(x, y, z) { max(0, ((x - y) - z))})(end_time, start_time, activity_time)
    ][,.(avg_wait = mean(wait_time,na.rm = T)),by = replication])
}

TB_obj_3 <- function(x) {
<<<<<<< HEAD
=======
TB_obj_2 <- function(x) {
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
=======
>>>>>>> a420328 (Git Repo updates)
  if (is.list(x)) {
    z <-
      x[[2]][, .(queue_length = mean(system)), by = list(resource, replication)][, .(max_mean_queue = max(queue_length)), by = replication]
    # return(x[[2]][,mean_server_utilization <- mean(utilization,na.rm = T), by  = replication]
  } else{
    z <-
      x[, .(time_std_dev = sd(activity_time, na.rm = T)), by = list(replication)]
  }
  return(z)
<<<<<<< HEAD
<<<<<<< HEAD
=======
}

TB_obj_3 <- function(x){
  #browser(expr = get('it',envir = .GlobalEnv))
  return(
    x[[1]][, wait_time := Vectorize(function(x, y, z) { max(0, ((x - y) - z))})(end_time, start_time, activity_time)
    ][,.(avg_wait = mean(wait_time,na.rm = T)),by = replication])
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
=======
>>>>>>> a420328 (Git Repo updates)
}