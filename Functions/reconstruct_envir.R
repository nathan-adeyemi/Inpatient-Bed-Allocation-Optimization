reconstruct_envir <- function(arg_list,.envir = parent.frame()) {
  
  # Recreates the environment from the most recent completed DB-PSA iteration
  
  # Inputs: arg_list (List) A list of arguments passed into the DB_PSA function that includes the algorithm history list.
  
  pareto_set <- arg_list$pareto_set
  A <- arg_list$A
  it <- length(A)
  temp <-
    cool_temp(
      initial_temperature = .envir$temp_init,
      alpha_param = .envir$t_damp,
      cool_sched = .envir$sched_type,
      current_iteration = it
    )
  best <- findBestbyDistance(pareto_set,.envir = .envir)
  best <- best$best
  
  all_allocations <-
    do.call(rbind, lapply(A, function(x)
      do.call(rbind, lapply(x, function(new_solns)
        as.matrix(t(new_solns %c% 'Allocation'))))))
  unique_sols <- unlist(lapply(seq_along(A),function(ind) unlist(A[[ind]],recursive = F)),recursive = F)
  unique_names <- which(!duplicated(unique_sols %c% 'name'))
  unique_sols <- unique_sols[unique_names]
  itReps_Cum <- sum(unique_sols %c% 'Replications') + .envir$initial_trials
  theoretical_cum <- .envir$N_Total * it
<<<<<<< HEAD
  pareto_counter <- sum(unlist(lapply(A, function(i) identical(A[[length(A)]]$itBest %c% 'name', i$itBest %c% 'name'))))
=======
>>>>>>> 2d8d6de (Further Updates)
  list2env(
    x = list(
      "A" = A,
      "pareto_set" = pareto_set,
<<<<<<< HEAD
      "it" = it + 1,
=======
      "it" = it,
>>>>>>> 2d8d6de (Further Updates)
      "temp" = temp,
      "best" = best,
      "all_allocations" = all_allocations,
      "itReps_Cum" = itReps_Cum,
<<<<<<< HEAD
      "theoretical_cum" = theoretical_cum,
      "pareto_counter" = pareto_counter,
      "pareto_limit" = 20
=======
      "theoretical_cum" = theoretical_cum
>>>>>>> 2d8d6de (Further Updates)
    ),
    envir = .envir
  )
}