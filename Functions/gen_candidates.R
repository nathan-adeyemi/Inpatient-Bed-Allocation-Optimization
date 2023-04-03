gen_candidates <-
  function(tweak_left = NA,
           .envir = parent.frame()) {
    # Function to generate and evaluate candidate solutions in the DB-PSA framework
    # also creates/appends a matri of all tested solutions
    
    # Function Inputs:
    #   tweak_left - the maximum number of candidate solutions to be evaluated 
    #                during a DB-PSA iteration
    
    
    temp_counter <- 0
    candidate_list <- list()
    tweak_left <- if(is.na(tweak_left)) .envir$nTweak
    while (tweak_left > 0 & temp_counter < 60) {
      new_solns <-
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
        as.matrix(new_solns %c% 'Allocation')
      )))
      new_solns = new_solns[new_alloc]
      
      # Remove any solution that was previously tested
      dups <-
        rbind(all_allocations,
              new_solns %c% 'Allocation' %>%
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
      if (length(dups) != length(new_solns)) {
        new_solns <-
          new_solns[setdiff(seq_along(new_solns), dups)]
      } else {
        new_solns = NULL
      }
      
      # Add all new generated solutions to the candidate list
      candidate_list <- append(candidate_list, new_solns)
      if (length(new_solns) > 0 & .envir$it != 0) {
        all_allocations <<-
          rbind(all_allocations, as.matrix(t(new_solns %c% 'Allocation')))
      }
      
      temp_counter %+% 1
      tweak_left <- tweak_left - length(candidate_list)
    }
    
    if (length(candidate_list) > 0) {
      for (i in seq_along(candidate_list)) {
        candidate_list[[i]]$name <-
          paste0('Tourney_', .envir$it, '_Candidate_', i)
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
      
      candidate_reps <<- sum(candidate_list %c% 'Replications')
    } else {
      candidate_list <- NULL
    }
    return(candidate_list)
  }
