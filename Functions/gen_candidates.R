gen_candidates <-
  function(tweak_left = NA,
           candidate_list = NULL,
           .envir = parent.frame()) {
    
    temp_counter <- 0
    new_solns <- list()
    tweak_left <- if(is.na(tweak_left)) .envir$nTweak
  
    while (tweak_left > 0 & temp_counter < 60) {
      candidate_list <-
        mclapply(
          seq(tweak_left),
          update_sol,
          mc.cores = availableCores(),
          .envir = .envir
        )
      # Remove duplicate allocations within temporary obj (inital subgroup)
      new_alloc = which({
        function(mat)
          ! duplicated(mat)
      }(t(
        as.matrix(candidate_list %c% 'Allocation')
      )))
      candidate_list = candidate_list[new_alloc]
      
      # Remove any solution that was previously tested
      dups <-
        rbind(all_allocations,
              candidate_list %c% 'Allocation' %>%
                t() %>%
                as.matrix()) %>%
        as.matrix() %>%
        {
          function(mat)
            which(duplicated(mat))
        }()
      if (length(dups) > 0) {
        dups <- dups - nrow(all_allocations)
      }
      if (length(dups) != length(candidate_list)) {
        candidate_list <-
          candidate_list[setdiff(seq_along(candidate_list), dups)]
      } else {
        candidate_list = NULL
      }
      
      # Add all new generated solutions to the candidate list
      new_solns = append(new_solns, candidate_list)
      if (length(candidate_list) > 0 & .envir$it != 0) {
        all_allocations <<-
          rbind(all_allocations, as.matrix(t(candidate_list %c% 'Allocation')))
      }
      
      temp_counter %+% 1
      tweak_left <- tweak_left - length(new_solns)
    }
    
    candidate_list <- new_solns
    for (i in seq_along(candidate_list)) {
      candidate_list[[i]]$name <- paste0('Tourney_', .envir$it, '_Candidate_', i)
    }
    # Make lists of all new solutions/allocations (1 entry per replication), a list of the replication #s,
    # and a list of the allocation's index in the candidate list
    allocation_list <-
      unlist(x = lapply(
        X = candidate_list,
        FUN = function(i) {
          rep(x = list(i$Allocation),
              times = i$Replications)
        }
      ),
      recursive = F)
    
    replication_list <-
      rep(x = seq(candidate_list[[1]]$Replications),
          times = length(candidate_list))
    
    index_list = sapply(
      seq_along(candidate_list),
      FUN = function(i)
        rep(x = i, times = candidate_list[[i]]$Replications)
    )
    
    if (length(new_solns) == 0 & temp_counter == 10) {
      candidate_list = NULL
    } else{
      test <- mclapply(
        X = seq_along(allocation_list),
        FUN = ocbaUpdate,
        mc.cores = availableCores(),
        arg_list = allocation_list,
        job_list = replication_list,
        .envir = .envir
      )
      candidate_list <- lapply(
        X = candidate_list,
        FUN = function(Object) {
          obj_ind = which(candidate_list %c% 'name' == Object$name)
          stats <- rbindlist(test[which(index_list == obj_ind)])
          Object <-
            updateSimStats(i = Object,
                           data = stats,
                           new_sol = T)
        }
      )
    }
    candidate_reps <<- sum(candidate_list %c% 'Replications')
    return(candidate_list)
  }
