pareto_perc_correct <-
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
  function(i,true_pSet = NA,size = NA_character_,extras = F) {
=======
  function(i,true_pSet,extras = F) {
>>>>>>> 9f23a66 (Function updates:)
=======
  function(i,true_pSet = NA,size = NA_character_,extras = F) {
>>>>>>> a420328 (Git Repo updates)
    
    # Function: Determines the percentage of solutions in the estimated Pareto set produced by the DB-PSA algorithm that are in the true Pareto set
    
    # Inputs:
    #   i - (List) Estimated Pareto set produced by DB-PSA
    #   true_pSet - (List) True Pareto set
<<<<<<< HEAD
<<<<<<< HEAD
    #   size - (Character) Indicates the size of the test network used (Only used if true_pSet is not)
=======
>>>>>>> 9f23a66 (Function updates:)
=======
    #   size - (Character) Indicates the size of the test network used (Only used if true_pSet is not)
>>>>>>> a420328 (Git Repo updates)
    #   extras - (Logical) Indicates if the function should find DB-PSA Pareto solutions which are not part of the true Pareto set.
    
    # Returns:
    #   correct_percentage - (Numeric) Percentage of true Pareto set solutions in the DB-PSA estimated Pareto Set
    #   non_pSet_sols - (Integer) Number of solutions in the estimated Pareto set non included in the true Pareto set.
    
<<<<<<< HEAD
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
=======
  function(i) {
    i <- conv_pSet_to_table(i)
    100 * (merge(
      x = i,
      y = true_pareto_set,
      by = colnames(true_pareto_set)
    )[, .N] /
      true_pareto_set[, .N])
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
=======
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
>>>>>>> 9f23a66 (Function updates:)
  }
