ocbaUpdate <-
  function(arg,
           arg_list,
           job_list,
           .envir = parent.frame()) {
    temp_results <-
      CostFunction(sol = arg_list[[arg]],
                   logic = FALSE,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 8c8946d (Fixed some custom functions.)
                   reps = 1,
                   .envir = .envir)
    temp_results <- objective_Metrics(temp_results,.envir = .envir)
    temp_results <- data.table(temp_results)[, replication := job_list[arg]]
<<<<<<< HEAD
=======
                   reps = 1)
    temp_results <-
      data.table(objective_Metrics(data = temp_results, fun_list = .envir$obj_function_list))[, replication := job_list[arg]]
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
>>>>>>> 8c8946d (Fixed some custom functions.)
    return(temp_results)
  }