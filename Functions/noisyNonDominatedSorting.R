noisyNonDominatedSorting <-
  function (inputData, .envir = parent.frame()) {
    #Modified from the fastNonDominatedSorting function in the nsga2R v1.1 package
    popSize = length(inputData)
    idxDominators = vector("list", popSize)
    idxDominatees = vector("list", popSize)
    for (i in 1:(popSize - 1)) {
      for (j in i:popSize) {
        if (i != j) {
          xi = inputData[[i]]
          xj = inputData[[j]]
          
          # Test if xi dominates xj
          crit_1 <- soln_comparison(s1 = xj,
                                    s2 = xi,
                                    stat = T)
          
          # Test if xj dominates xi
          crit_2 <- soln_comparison(s1 = xi,
                                    s2 = xj,
                                    stat = T)
          
          # Assign dominating and dominated solutions
          if (crit_1 && !crit_2) {
            idxDominators[[j]] = c(idxDominators[[j]],
                                   i)
            idxDominatees[[i]] = c(idxDominatees[[i]],
                                   j)
          } else if (crit_2 && !crit_1) {
            idxDominators[[i]] = c(idxDominators[[i]],
                                   j)
            idxDominatees[[j]] = c(idxDominatees[[j]],
                                   i)
          }
        }
      }
    }
    
    noDominators <- lapply(idxDominators, length)
    rnkList <- list()
    rnkList <- c(rnkList, list(which(noDominators == 0)))
    solAssigned <- c()
    solAssigned <- c(solAssigned, length(which(noDominators ==
                                                 0)))
    while (sum(solAssigned) < popSize) {
      Q <- c()
      noSolInCurrFrnt <- solAssigned[length(solAssigned)]
      if (noSolInCurrFrnt > 0) {
        for (i in 1:noSolInCurrFrnt) {
          solIdx <- rnkList[[length(rnkList)]][i]
          hisDominatees <- idxDominatees[[solIdx]]
          for (i in hisDominatees) {
            noDominators[[i]] <- noDominators[[i]] - 1
            if (noDominators[[i]] == 0) {
              Q <- c(Q, i)
            }
          }
        }
        rnkList <- c(rnkList, list(sort(Q)))
        solAssigned <- c(solAssigned, length(Q))
      }
    }
    rnkVec <- rnkList
    names(rnkVec) <- seq_along(rnkVec)
    rnkVec <- as.numeric(data.table(stack(rnkVec))[order(values), ind])
    return(list('rnkList' = rnkList,
                'rnkVec' = rnkVec))
  }
