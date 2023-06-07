mcnsga2 <- function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
          upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
          generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
          MuDistIdx = 10,num_replications,results_directory = NA_character_,.envir = parent.frame()){
  # Modified version of the nsga2 function in the nsga2R package version 1.1
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  # cat("initializing the population")
  # cat("\n")
  
  results_directory <- file.path(results_directory,'NSGA_Generations')
  if(!dir.exists(results_directory)){
    dir.create(results_directory)
  }
  
  parent <- t(sapply(1:popSize, function(u) array(runif(length(lowerBounds), 
                                                        lowerBounds, upperBounds))))
  
  parent_results <-
    nsga2SimReplications(
      sol_matrix = parent,
      nRep = num_replications,
      obj_fun = fn,
      .envir = .envir
    )

    parent <- cbind(parent,
                  t(sapply(
                    X = parent_results,
                    FUN = function(i) {
                      return(apply(i, 2, mean))
                    }
                  )))
  ranking_list <- noisyNonDominatedSorting(inputData = parent_results,.envir = .envir)
  ranking <- ranking_list$rnkList
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  # cat("crowding distance calculation")
  # cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    cat("---------------generation---------------", iter, "starts")
    cat("\n")
    # cat("tournament selection")
    # cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    # cat("crossover operator")
    # cat("\n")
    childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    # cat("mutation operator")
    # cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    # cat("evaluate the objective fns of childAfterM")
    # cat("\n")
    childAfterM <- childAfterM[,1:varNo]
    
    children_results <-
      nsga2SimReplications(
        sol_matrix = childAfterM,
        nRep = num_replications,
        obj_fun = fn,
        .envir = .envir
      )
    
    childAfterM <- cbind(childAfterM,
                    t(sapply(
                      X = children_results,
                      FUN = function(i) {
                        return(apply(i, 2, mean))
                      }
                    )))
    
    # cat("Rt = Pt + Qt")
    # cat("\n")
    parentNext <- rbind(parent[,seq(ncol(childAfterM))],childAfterM)
    # cat("ranking again")
    # cat("\n")
    ranking_list <- noisyNonDominatedSorting(inputData = append(parent_results,children_results),.envir = .envir)
    ranking <- ranking_list$rnkList
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    # cat("crowded comparison again")
    # cat("\n")
    
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
    ]
    # cat("environmental selection")
    # cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    # cat("---------------generation---------------", iter, "ends")
    # cat("\n")
    if (iter != generations) {
      # cat("\n")
      # cat("********** new iteration *********")
      # cat("\n")
      save(list = ls(all.names = T),
           file = file.path(
        results_directory,
        paste('Generation', iter, "envir.Rdata", sep = "_")
      ))
    }
  }
  parent[,(varNo + 1)] <- -1 * parent[,(varNo + 1)]
  result = list(
    functions = fn,
    parameterDim = varNo,
    objectiveDim = objDim,
    lowerBounds = lowerBounds,
    upperBounds = upperBounds,
    popSize = popSize,
    tournamentSize = tourSize,
    generations = generations,
    XoverProb = cprob,
    XoverDistIndex = XoverDistIdx,
    mutationProb = mprob,
    mutationDistIndex = MuDistIdx,
    parameters = parent[,
                        1:varNo],
    objectives = parent[, (varNo + 1):(varNo +
                                         objDim)],
    paretoFrontRank = parent[, varNo + objDim +
                               1],
    crowdingDistance = parent[, varNo + objDim +
                                2],
    sol_samples = append(parent_results, children_results)[order(parentNext[, varNo + objDim + 1],-parentNext[, varNo + objDim + 2])][seq(popSize)]
    )
  class(result) = "nsga2R"
  return(result)
}