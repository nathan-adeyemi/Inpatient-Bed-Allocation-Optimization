ocbaUpdate <-
  function(arg,
           arg_list,
           job_list,
           .envir = parent.frame()) {
    temp_results <-
      CostFunction(sol = arg_list[[arg]],
                   logic = FALSE,
                   reps = 1)
    temp_results <-
      data.table(objective_Metrics(data = temp_results, fun_list = .envir$obj_function_list))[, replication := job_list[arg]]
    return(temp_results)
  }