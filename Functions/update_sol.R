update_sol <- function(i,sol_vector = NA, .envir = parent.frame()){
  x <- .envir$best
  if(length(x) == 1){
    x <- x[[1]]
  }
  if (any(is.na(sol_vector))) {
    x$Replications <- .envir$initial_trials
    x$counter <- 0
    x$Solution <-  tweak(x$Solution,.envir = .envir)
  } else{
    x$name <-
      paste0("test_nsga_sol_",i)
    x$Solution <- sol_vector
    x$Replications <- 20
  }
  x$Allocation <- decode(x$Solution,.envir = .envir)
  return(x)
}
