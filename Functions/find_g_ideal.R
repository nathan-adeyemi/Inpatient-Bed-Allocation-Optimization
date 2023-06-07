find_g_ideal <- function(pSet, .envir = parent.frame(),dist = T) {
  
  # Find the sample statistics of individual solutions to form the ideal reference point
  
  # Inputs: 
  #   pSet - (List) The current Pareto Set
  
  # Returns:
  #   g_ideal_dist - (Matrix) a matrix where each column is the sample statistics of the solution in the Pareto set 
  #                           with the minimum expected value for that associated objective function
  
  obj_means <- apply(
    X = as.matrix(t(pSet %c% 'Obj_mean')),
    MARGIN = 2,
    FUN = function(u)
      matrix(unlist(u))
  )
  if(dist){
  g_ideal_dist <- sapply(
    X = seq(ncol(obj_means)),
    FUN = function(index)
      eval(parse(
        text = paste0('which.', .envir$optim_type[index], '(obj_means[,', index, '])')
      ))
  )
  g_ideal_dist <-
    sapply(
      X = seq_along(g_ideal_dist),
      FUN = function(i)
        as.matrix(pSet[[g_ideal_dist[i]]]$Cost)[, i + 1]
    )
  
  if (is.list(g_ideal_dist)) {
    multiple <- Reduce(Lcm, sapply(g_ideal_dist, length))
    g_ideal_dist <<- g_ideal_dist <-
      sapply(g_ideal_dist, function(i)
        unlist(rep(i, multiple / length(i))))
  }
  return(g_ideal_dist)
  } else {
    g_ideal_means <- sapply(
      X = seq_along(.envir$optim_type),
      FUN = function(index) {
        eval(parse(
          text = paste0(.envir$optim_type[index], '(obj_means[,', index, '])')
        ))
      }
    )
    return(g_ideal_means)
  }
}