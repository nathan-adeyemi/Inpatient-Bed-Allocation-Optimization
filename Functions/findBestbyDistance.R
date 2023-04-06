findBestbyDistance <-
  function(pSet, 
           rankInfo, 
           .envir = parent.frame()) {
    if (length(pSet) > 1) {
      g_ideal_dist <- find_g_ideal(pSet = pSet,.envir = .envir)
      g_ideal_CI <<-
        apply(
          X = g_ideal_dist,
          MARGIN = 2,
          FUN = function(i)
            ci_as_text(t.test(i, conf.level = 0.95)$conf.int)
        )
      
      # divergences <- sapply(
      #   X = pSet,
      #   FUN = function(i) {
      #     sd_cols <- setdiff(colnames(i$Cost), 'replication')
      #     return(kldiv(
      #       mu1 = apply(g_ideal_dist, 2, mean),
      #       mu2 = i$Cost[, sapply(.SD, mean), .SDcols = sd_cols],
      #       sigma1 = cov(g_ideal_dist),
      #       sigma2 = cov(i$Cost[, -1]),
      #       symmetrized = T
      #     ))
      #   }
      # )
      divergences <- sapply(
        X = pSet,
        FUN = function(i) {
          sd_cols <- setdiff(colnames(i$Cost), 'replication')
          # return(kldiv(
          return(bhattacharyya.dist(
            mu1 = apply(g_ideal_dist, 2, mean),
            mu2 = i$Cost[, sapply(.SD, mean), .SDcols = sd_cols],
            Sigma1 = cov(g_ideal_dist),
            Sigma2 = cov(i$Cost[,-1])
          ))
        }
      )
      
      selection_probs <-
        `if`(length(pSet) > 2, mod_softmax(scale(divergences)), rep(.5, 2))
      
      pSet <- lapply(
          X = seq_along(pSet),
          FUN = function(i) {
            pSet[[i]]$Divergence <- divergences[i]
            pSet[[i]]$P_Selection <-
              round(selection_probs[i], digits = 4)
            return(pSet[[i]])
          }
        )
      
      best <-
        sample(pSet,
               size = 1,
               prob = selection_probs)
      
    } else {
      g_ideal_CI <<- unlist(.envir$best$Obj_CI)
      best <- pSet[[1]]
    }
    return(list(`best` = best,`pareto_set` = pSet))
  }
