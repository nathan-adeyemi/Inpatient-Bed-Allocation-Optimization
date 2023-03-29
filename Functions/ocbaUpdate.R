ocbaUpdate <-
  function(arg,
           arg_list,
           job_list,
           .envir = parent.frame()) {
    temp_results <-
      CostFunction(sol = arg_list[[arg]],
                   logic = FALSE,
                   reps = 1,
                   .envir = .envir)
    temp_results <- objective_Metrics(temp_results,.envir = .envir)
    temp_results <- data.table(temp_results)[, replication := job_list[arg]]
    return(temp_results)
  }