gridSearchSubFn <- function(unique_allocations, nRep, .envir) {
  
  if(is.data.table(unique_allocations)){
    unique_allocations <- mclapply(
      X = seq(nrow(unique_allocations)),
      FUN = function(i)
        list("Allocation" = unlist(unique_allocations[i,])),
      mc.cores = availableCores()
    )
    
    unique_allocations <- mclapply(
      X = seq_along(unique_allocations),
      FUN = function(i) {
        unique_allocations[[i]]$name <- paste0("Allocation_", i)
        return(unique_allocations[[i]])
      },
      mc.cores = availableCores()
    )
  }
  
  allocation_list <-
    unlist(x = mclapply(
      X = unique_allocations,
      FUN = function(i) {
        rep(x = list(i$Allocation), times = nRep)
      },
      mc.cores = availableCores()),
      recursive = F)
  
  replication_list <-
    rep(x = seq(nRep),
        times = length(unique_allocations))
  
  index_list <- as.vector(sapply(
    seq_along(unique_allocations),
    FUN = function(i) {
      rep(x = i, times = nRep)
    }
  ))
  
  test <- mclapply(
    X = seq_along(allocation_list),
    FUN = ocbaUpdate,
    mc.cores = availableCores(),
    arg_list = allocation_list,
    job_list = replication_list,
    .envir = .envir
  )
  # return( list(
  #   `possible_allocations` = unique_allocations,
  #   `replication_list` = replication_list,
  #   `allocation_list` = allocation_list,
  #   `test` = test,
  #   `index_list` = index_list
  # ))
  
  
  # Sorting solutions into Pareto and other fronts (Bi-Objective)
  
  # test <- lapply(test,function(i){
  #   i[,ncol(i) := NULL]
  # })
  
  optim_type <- c('max','min')
  
  possible_allocs <- mclapply(
    X = possible_allocs,
    FUN = function(Object) {
      obj_ind <- which(possible_allocs %c% "name" == Object$name)
      stats <- rbindlist(test[which(index_list == obj_ind)])
      Object <-
        updateSimStats(i = Object,
                       data = stats,
                       new_sol = T)
    },
    mc.cores = availableCores()
  )
  return(possible_allocs)
}