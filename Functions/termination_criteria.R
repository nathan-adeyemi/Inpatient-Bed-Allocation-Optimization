termination_criteria <-
<<<<<<< HEAD
<<<<<<< HEAD
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
=======
  function(check_pareto = F,
>>>>>>> d020c10 (Updated function descriptions for some of the DB_PSA functions.)
           check_iteration = F,
           check_temp = T,
           .envir = parent.frame()) {
    
=======
  function(.envir = parent.frame()) {
>>>>>>> 9f23a66 (Function updates:)
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
<<<<<<< HEAD
<<<<<<< HEAD
    } else {
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
    } else if(check_temp){
>>>>>>> d020c10 (Updated function descriptions for some of the DB_PSA functions.)
=======
    } else{
>>>>>>> 9f23a66 (Function updates:)
      logical <- .envir$temp > .envir$t_min
    }
    return(logical)
  }