ndsa <- function(pop, envir = parent.frame()){
  allFronts = list()
  frontRank <- 1
  
  while (length(pop) > 1){
    front <- list(pop[[1]])
    popPrime <- pop
    pop <- pop[2:length(pop)]
    front_ind <- c(1) # front_ind is the is current front's solution's index in the overall population of results (pop)
    
    for (i in seq_along(pop)){
      front <- c(front,list(pop[[i]]))
      front_ind <- c(front_ind,i+1)
      end <- length(front)
      frontRemoveIndex <- c() #frontRemoveIndex is the index of a solution in the current front NOT the population
      
      for (j in seq_along(front)[-end]){
        #If the added solution is dominated by any solution in the current front, remove it and break the loop
        paretoTest1 <- soln_comparison(front[[end]],front[[j]]) %>% {function(vec) sum(vec) == length(vec)}()
        
        # If a solution in the current front is dominated by the tested solution, remove that solution in the current front and add the other
        paretoTest2 <- soln_comparison(front[[j]],front[[end]]) %>% {function(vec) sum(vec) == length(vec)}()
        if(paretoTest1){ 
          if(!(end %in% frontRemoveIndex)){
            frontRemoveIndex <- c(frontRemoveIndex,end)
          }
        }else if (paretoTest2){ 
          frontRemoveIndex <- c(frontRemoveIndex,j)
        }
      }
      
      # Remove dominated solutions from current front and front index list
      front[frontRemoveIndex] <- NULL
      if(!is.null(frontRemoveIndex)){
        front_ind <- front_ind[-frontRemoveIndex]
      }
    }
    allFronts[paste0('Front_',frontRank)] <- list(front)
    pop <- popPrime[-front_ind]
    front <- list()
    frontRank <- frontRank %>%  sum(1)
  }
  return(allFronts)
}
