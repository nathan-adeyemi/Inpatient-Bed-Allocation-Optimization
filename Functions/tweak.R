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
    changes <-
      as.numeric(runif(.envir$nVar) < p_accept(.envir = .envir))
  }
  changes <-
    runif(n = length(changes),
          min = -change_lim,
          max = change_lim) * changes
  x <- sapply(x + changes, fix_cell)
  return(x)
}