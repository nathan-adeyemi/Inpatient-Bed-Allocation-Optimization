termination_criteria <-
<<<<<<< HEAD
  function(.envir = parent.frame()) {
    # Function: Tells the main optimization function (DB_PSA) whether to continue
    
    # Inputs:
    #   check_temp: (Logical) - Default - Termination decided by whether the current temperature is below a prespecified minimum (requires the t_min DB_PSA argument)
    #   check_pareto: (Logical) - Termination decided by whether the Pareto set has remained unchanged for a specified number of iterations (requires the pareto_limit argument in DB_PSA)
    #   check_iteration: (Logical) - Termination decided by whether the prespecified maximum number of DB_PSA iterations has been occurred (requires the itMax argument in DB_PSA)
    
    # Return (Logical):
    #   TRUE: Continue to next iteration
    #   FALSE: Terminate algorithm and return results
    
    if (.envir$it == 1) {
      logical <- T
    } else if (!is.na(.envir$pareto_limit)) {
      logical <-
        .envir$pareto_counter < .envir$pareto_limit
    } else if (!is.na(.envir$itMax)) {
      logical <- .envir$it < .envir$itMax
    } else{
=======
  function(eval_gradient = F,
           check_pareto = F,
           check_iteration = F,
           .envir = parent.frame()) {
    obj_names <- colnames(.envir$best$Obj_mean)
    if (.envir$it < 1) {
      logical <- T
    } else if (eval_gradient) {
      # Defines the minimum gradient stopping criteria
      grad_term <-
        data.table(matrix(c(0, 0.5, 0, -0.05, 0, -0.05), nrow = 2))
      
      #Names the gradient terminating criteria dataframe columns
      colnames(grad_term) <- obj_names
      
      gradient <-
        tail(tail(unique(.envir$best_df[, ..obj_names]), n = 2)[, lapply(
          X = .SD,
          FUN = function(i) {
            100 * ((i - data.table::shift(i, n = 1)) / data.table::shift(i, n = 1))
          }
        ), .SDcols = obj_names],
        n = 1)
      
      logical <-
        !all(sapply(
          X = obj_names,
          FUN = function(column) {
            return(min(gradient_term[, ..column]) < gradient[, ..column] &
                     max(gradient_term[, ..column]) > gradient[, ..column])
          }
        ) == T)
    } else if (check_pareto) {
      logical <-
        any(.envir$temp > .envir$t_min,
            .envir$pareto_counter < .envir$pareto_limit)
    } else if (check_iteration) {
      logical <- .envir$it < .envir$itMax
    } else {
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
      logical <- .envir$temp > .envir$t_min
    }
    return(logical)
  }