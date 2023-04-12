update_sol <- function(i,sol_vector = NA, .envir = parent.frame()){
  x <- .envir$best
<<<<<<< HEAD
<<<<<<< HEAD
  if(length(x) == 1){
    x <- x[[1]]
  }
  if (any(is.na(sol_vector))) {
    x$Replications <- .envir$initial_trials
=======
  if (any(is.na(sol_vector))) {
    x$Replications <- .envir$starter_reps
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
  if(length(x) == 1){
    x <- x[[1]]
  }
  if (any(is.na(sol_vector))) {
    x$Replications <- .envir$initial_trials
>>>>>>> f4d354d (Further Updates:)
    x$counter <- 0
    x$Solution <-  tweak(x$Solution,.envir = .envir)
  } else{
    x$name <-
      paste0("test_nsga_sol_",i)
    x$Solution <- sol_vector
<<<<<<< HEAD
    x$Replications <- 20
=======
    x$Replications <- 12
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
  }
  x$Allocation <- decode(x$Solution,.envir = .envir)
  return(x)
}
