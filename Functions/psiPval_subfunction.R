psiPval_subfunction <-
  function(i, means, deviations, .envir = parent.frame()) {
    z <- pnorm(q = as.numeric(means[, ..i]),
               mean = 0,
               sd = as.numeric(deviations[, ..i]))
    if (.envir$optim_type[which(colnames(means) == i)] == 'max') {
      z <- 1 - z
    }
    return(z)
  }