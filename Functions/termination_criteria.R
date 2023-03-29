termination_criteria <-
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
      logical <- .envir$temp > .envir$t_min
    }
    return(logical)
  }