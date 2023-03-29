findBestbyDistance <-
  function(pSet, 
           rankInfo, 
           .envir = parent.frame()) {
    if (length(pSet) > 1) {
      obj_means <- apply(
        X = as.matrix(t(pSet %c% 'Obj_mean')),
        MARGIN = 2,
        FUN = function(u)
          matrix(unlist(u))
      )
      
      g_ideal_dist <- sapply(
        X = seq(ncol(obj_means)),
        FUN = function(index)
          eval(parse(
            text = paste0('which.', optim_type[index], '(obj_means[,', index, '])')
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
        g_ideal_dist <-
          sapply(g_ideal_dist, function(i)
            unlist(rep(i, multiple / length(i))))
      }
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
          return(bhattacharyya.dist(
            mu1 = apply(g_ideal_dist, 2, mean),
            mu2 = i$Cost[, sapply(.SD, mean), .SDcols = sd_cols],
            Sigma1 = cov(g_ideal_dist),
            Sigma2 = cov(i$Cost[,-1])
          ))
        }
      )
      
      selection_probs <- mod_softmax(scale(divergences))
      pareto_set <<-
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
      g_ideal_CI <<- unlist(best$Obj_CI)
      best <- pSet[[1]]
    }
    return(best)
  }
