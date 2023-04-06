findBestbyDistance <-
  function(pSet, 
           rankInfo, 
           .envir = parent.frame()) {
    if (length(pSet) > 1) {
<<<<<<< HEAD
<<<<<<< HEAD
      g_ideal_dist <- find_g_ideal(pSet = pSet,.envir = .envir)
=======
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
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
      g_ideal_dist <- find_g_ideal(pSet = pSet,.envir = .envir)
>>>>>>> 9f23a66 (Function updates:)
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
<<<<<<< HEAD
<<<<<<< HEAD
          # return(kldiv(
=======
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
          # return(kldiv(
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
          return(bhattacharyya.dist(
            mu1 = apply(g_ideal_dist, 2, mean),
            mu2 = i$Cost[, sapply(.SD, mean), .SDcols = sd_cols],
            Sigma1 = cov(g_ideal_dist),
            Sigma2 = cov(i$Cost[,-1])
          ))
        }
      )
      
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
      selection_probs <-
        `if`(length(pSet) > 2, mod_softmax(scale(divergences)), rep(.5, 2))
      
      pSet <- lapply(
<<<<<<< HEAD
=======
      selection_probs <- mod_softmax(scale(divergences))
      pareto_set <<-
        pSet <- lapply(
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
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
<<<<<<< HEAD
<<<<<<< HEAD
      g_ideal_CI <<- unlist(.envir$best$Obj_CI)
      best <- pSet[[1]]
    }
    return(list(`best` = best,`pareto_set` = pSet))
=======
      g_ideal_CI <<- unlist(best$Obj_CI)
      best <- pSet[[1]]
    }
    return(best)
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
      g_ideal_CI <<- unlist(.envir$best$Obj_CI)
      best <- pSet[[1]]
    }
    return(list(`best` = best,`pareto_set` = pSet))
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
  }
