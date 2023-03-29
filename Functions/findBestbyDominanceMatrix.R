findBestbyDominanceMatrix <-
  function(current_best,
           candidate_list,
           stat_comp = F,
           envir = parent.frame()) {
    
    # Creates dominance matrices
    # Key:
    # N_Objectives = row solution dominates column solution
    # (1:N_objectives-1) = row solution and column solution are pareto equivalent
    # 0 = row solution is dominated by column solution
    
    dominance_matrix <- dominance_matrix_non_stat <-
      matrix(
        data = NA,
        nrow = length(candidate_list),
        ncol = length(candidate_list)
      )
    
    for (i in seq_along(candidate_list)) {
      for (j in seq_along(candidate_list)) {
        if (i != j) {
          dominance_matrix[i, j] <-
            sum(!soln_comparison(
              s1 = candidate_list[[i]],
              s2 = candidate_list[[j]],
              stat = stat_comp
            ))
        }
      }
    }
    
    if (length(candidate_list) >= 2) {
      idx <- which.max(rowSums(dominance_matrix, na.rm = T) /
                         (ncol(dominance_matrix) - 1))
    } else {
      idx = 1
    }
    
    if (length(idx) > 1) {
      idx <- idx[which.min(candidate_list[idx] %c% 'Psi')]
    }
    
    if (!identical(candidate_list[[idx]], best)) {
      Prev_Best <- current_best
      best <- candidate_list[[idx]]
    } else {
      Prev_Best <- current_best
      best <- current_best
    }
    return(best)
  }