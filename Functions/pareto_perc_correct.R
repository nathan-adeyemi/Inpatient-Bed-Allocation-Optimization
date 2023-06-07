pareto_perc_correct <-
  function(i,true_pSet = NA,size = NA_character_,extras = F) {
    
    # Function: Determines the percentage of solutions in the estimated Pareto set produced by the DB-PSA algorithm that are in the true Pareto set
    
    # Inputs:
    #   i - (List) Estimated Pareto set produced by DB-PSA
    #   true_pSet - (List) True Pareto set
    #   size - (Character) Indicates the size of the test network used (Only used if true_pSet is not)
    #   extras - (Logical) Indicates if the function should find DB-PSA Pareto solutions which are not part of the true Pareto set.
    
    # Returns:
    #   correct_percentage - (Numeric) Percentage of true Pareto set solutions in the DB-PSA estimated Pareto Set
    #   non_pSet_sols - (Integer) Number of solutions in the estimated Pareto set non included in the true Pareto set.
    
    i <- conv_pSet_to_table(i)
    if(is.na(true_pSet) & !is.na(size)){
      true_env <- new.env()
      load(file.path("Data","Test Networks True Pareto Sets",size,paste(size,"True Pareto Set.rdata")),envir = true_env)
      true_pareto_set <- conv_pSet_to_table(true_env$pareto_set[[1]])
    }
    if(!extras) {
      return(100 * (merge(
        x = i,
        y = true_pareto_set,
        by = colnames(true_pareto_set)
      )[, .N]/true_pareto_set[, .N]))
    } else{
      return(suppressMessages(dplyr::anti_join(i,true_pareto_set)[,.N]))
    }
  }
