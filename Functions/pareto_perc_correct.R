pareto_perc_correct <-
  function(i,true_pSet,extras = F) {
    
    # Function: Determines the percentage of solutions in the estimated Pareto set produced by the DB-PSA algorithm that are in the true Pareto set
    
    # Inputs:
    #   i - (List) Estimated Pareto set produced by DB-PSA
    #   true_pSet - (List) True Pareto set
    #   extras - (Logical) Indicates if the function should find DB-PSA Pareto solutions which are not part of the true Pareto set.
    
    # Returns:
    #   correct_percentage - (Numeric) Percentage of true Pareto set solutions in the DB-PSA estimated Pareto Set
    #   non_pSet_sols - (Integer) Number of solutions in the estimated Pareto set non included in the true Pareto set.
    
    i <- conv_pSet_to_table(i)
    if(!extras) {
      return(100 * (merge(
        x = i,
        y = true_pareto_set,
        by = colnames(true_pareto_set)
      )[, .N] /
        true_pareto_set[, .N]))
    } else{
      return(suppressMessages(anti_join(i,true_pareto_set)[,.N]))
    }
  }
