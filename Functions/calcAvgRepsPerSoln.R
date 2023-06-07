calcAvgRepsPerSoln <- function(file) {
  A <- readRDS(file)
  A <- A$history
  avg_reps <- unique(rbindlist(lapply(
    X = seq_along(A),
    FUN =  function(ind)
      rbindlist(lapply(
        X = c('itBest', 'Rejects'),
        FUN = function(group) {
          x <- data.table(sapply(
              X = c('name', 'Replications'),
              FUN = function(i) {
              x <- A[[ind]][[group]] %c% i
              return(x)
              }
          ))
          return(x)
        }
      )))
  ))[, .(avg = mean(as.numeric(Replications)))]
  return(avg_reps)
}