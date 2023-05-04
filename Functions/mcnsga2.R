mcnsga2 <- function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
          upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
          generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
<<<<<<< HEAD
<<<<<<< HEAD
          MuDistIdx = 10,num_replications,results_directory = NA_character_,.envir = parent.frame()){
=======
          MuDistIdx = 10,.envir = parent.frame()){
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
          MuDistIdx = 10,num_replications,results_directory = NA_character_,.envir = parent.frame()){
>>>>>>> a420328 (Git Repo updates)
  # Modified version of the nsga2 function in the nsga2R package version 1.1
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  # cat("initializing the population")
  # cat("\n")
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
  
  results_directory <- file.path(results_directory,'NSGA_Generations')
  if(!dir.exists(results_directory)){
    dir.create(results_directory)
  }
  
<<<<<<< HEAD
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
=======
=======
>>>>>>> a420328 (Git Repo updates)
  parent <- t(sapply(1:popSize, function(u) array(runif(length(lowerBounds), 
                                                        lowerBounds, upperBounds))))
  
<<<<<<< HEAD
  # cat("ranking the initial population")
  # cat("\n")
  ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + 
                                                             objDim)])
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
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
>>>>>>> a420328 (Git Repo updates)
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
    childAfterM <- childAfterM[,1:varNo]
    
    children_results <-
      nsga2SimReplications(
        sol_matrix = childAfterM,
        nRep = num_replications,
        obj_fun = fn,
        .envir = .envir
      )
    
<<<<<<< HEAD
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
=======
=======
>>>>>>> a420328 (Git Repo updates)
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
<<<<<<< HEAD
    ranking <- fastNonDominatedSorting(parentNext[, (varNo + 
                                                       1):(varNo + objDim)])
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
    ranking_list <- noisyNonDominatedSorting(inputData = append(parent_results,children_results),.envir = .envir)
    ranking <- ranking_list$rnkList
>>>>>>> a420328 (Git Repo updates)
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    # cat("crowded comparison again")
    # cat("\n")
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
    
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    
<<<<<<< HEAD
=======
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
>>>>>>> a420328 (Git Repo updates)
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
      save(list = ls(all.names = T),
           file = file.path(
        results_directory,
        paste('Generation', iter, "envir.Rdata", sep = "_")
      ))
<<<<<<< HEAD
=======
    }
    else {
      # cat("********** stop the evolution *********")
      # cat("\n")
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
>>>>>>> a420328 (Git Repo updates)
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
<<<<<<< HEAD
<<<<<<< HEAD
                                2],
    sol_samples = append(parent_results, children_results)[order(parentNext[, varNo + objDim + 1],-parentNext[, varNo + objDim + 2])][seq(popSize)]
    )
=======
                                2])
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
                                2],
    sol_samples = append(parent_results, children_results)[order(parentNext[, varNo + objDim + 1],-parentNext[, varNo + objDim + 2])][seq(popSize)]
    )
>>>>>>> a420328 (Git Repo updates)
  class(result) = "nsga2R"
  return(result)
}