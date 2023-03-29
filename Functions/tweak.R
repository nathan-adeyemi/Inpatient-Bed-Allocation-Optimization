tweak <- function(x,
                  limit_change = F,
                  .envir = parent.frame()) {
  change_lim <- sl_ifelse(
    test = limit_change,
    yes = (.envir$temp / .envir$temp_init * .envir$maxChange),
    no = .envir$maxChange
  )
  
  changes <- rep(0, .envir$nVar)
  while (sum(changes) == 0) {
<<<<<<< HEAD
    changes <-
      as.numeric(runif(.envir$nVar) < p_accept(.envir = .envir))
=======
    changes <- as.numeric(runif(.envir$nVar) < p_accept(.envir$it))
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
  }
  changes <-
    runif(n = length(changes),
          min = -change_lim,
          max = change_lim) * changes
  x <- sapply(x + changes, fix_cell)
  return(x)
}