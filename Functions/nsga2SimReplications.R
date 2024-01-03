nsga2SimReplications <- function(sol_matrix, nRep, obj_fun, .envir = parent.frame()) {
  allocation_list <-
    matrix(unlist(lapply(
      X = seq(nrow(sol_matrix)),
      FUN = function(i)
        lapply(
          X = seq(nRep),
          FUN = function(c)
            sol_matrix[i, ]
        )
    )), ncol = .envir$n_queues, byrow = T)
  
  allocation_list <- lapply(
    X = seq(nrow(allocation_list)),
    FUN = function(row) {
      return(decode(allocation_list[row, ],.envir = .envir))
    }
  )
  
  replication_list <-
    rep(x = seq(nRep), times = nrow(sol_matrix))
  
  index_list = sapply(
    seq(nrow(sol_matrix)),
    FUN = function(i)
      rep(x = i, times = nRep)
  )
  test <- mclapply(
    X = seq_along(allocation_list),
    FUN = function(x, y, z, env) {
      return(do.call(
        what = obj_fun,
        args = list(
          arg = x,
          arg_list = y,
          job_list = z,
          .envir = env
        )
      ))
    },
    mc.cores = availableCores(),
    y = allocation_list,
    z = replication_list,
    env = .envir
  )
  parent_results <- lapply(
    X = seq(nrow(sol_matrix)),
    FUN = function(par_ind) {
      return(rbindlist(test[which(index_list == par_ind)]
      )[,replication := NULL])
    }
  )
  return(parent_results)
}