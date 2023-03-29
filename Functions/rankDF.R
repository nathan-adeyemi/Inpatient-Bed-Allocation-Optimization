rankDF <- function(rank, .envir = parent.frame()) {
  count = 1
  dt <-
    data.frame(matrix(
      data = 0,
      ncol = length(best$Obj_mean),
      nrow = length(allFronts[[rank]])
    ))
  names(dt) <- names(best$Obj_mean)
  for (i in allFronts[[paste0('Front_', rank)]]) {
    dt[count, ] <- i$Obj_mean
    count <- count %>%  sum(1)
  }
  dt$rank <- rank
  return(dt)
}