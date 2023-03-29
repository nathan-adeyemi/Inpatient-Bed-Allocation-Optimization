updateHistory <-
  function(candidate_list,
           history = NA,
           .envir = parent.frame()) {
    history <- `if`(is.na(history), .envir$A, history)
    if (length(history) != 0) {
      history <- append(history,
                        list(list(
                          Rejects = setdiff(candidate_list, .envir$pareto_set),
                          itBest = .envir$pareto_set
                        )))
    } else{
      history <- list(list(
        Rejects = setdiff(candidate_list, .envir$pareto_set),
        itBest = .envir$pareto_set
      ))
    }
    return(history)
  }