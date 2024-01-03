findBestbySequentialComparison <- function(candidate_list,
                                           stat_comp = get('stat_logical'), .envir = parent.frame()) {
  for (i in candidate_list) {
    # Objective Functions Statistical Tests (Non - parametric test)
    test <-  soln_comparison(best, i, stat = stat_comp)
    
    psi_diff <-
      (list(best, i) %c% 'Psi') %>% {
        function(i)
          max(i) - min(i)
      }()
    grad_change <-
      (list(best, i) %c% 'Obj_mean') %>% {
        function(i)
          (i[, 2] - i[, 1]) / i[, 1]
      }()
    psiList <- append(psiList, psi_diff)
    test_val <- runif(1)
    comp_val <- p_accept(sum(!test), psi_diff, temp, T)
    if (test_val < comp_val) {
      # Add previous best to archive
      Prev_Best <- append(Prev_Best, list(best))
      # Assign the surviving solution as the newest "best" solution
      best <- i
    } else {
      Rejected <- append(Rejected, list(i))
    }
  }
  return(best)
}
