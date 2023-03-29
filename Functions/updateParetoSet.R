updateParetoSet <-
  function(pSet, candidate_list, .envir = parent.frame()) {
    pSet <-  append(pSet, candidate_list)
    pSet <- removeDuplicateSolutions(pSet)
    if (length(pSet) > 1) {
      ranks <-
        noisyNonDominatedSorting(inputData = pSet, .envir = .envir)
    } else {
      ranks <- list(ranks = 1, rnkList = list(1))
    }
    pSet <- pSet[ranks$rnkList[[1]]]
    return(list('pSet' = pSet, 'ranks' = ranks))
  }