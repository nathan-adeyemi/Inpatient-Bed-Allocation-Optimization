soln_comparison <-
  function(s1,
           s2,
           alpha = 0.05,
           .envir = parent.frame()) {
    if(any(class(s1) == 'data.table')) {
      comp_df <- rbind(s1[, soln_num := 1],
                       s2[, soln_num := 2])
    } else {
      comp_df <- rbind(copy(s1$Cost)[, soln_num := 1],
                       copy(s2$Cost)[, soln_num := 2])
    }
    alpha <- 0.05
    #Less strict alpha value if there are few simulation replications
    if (any(sapply(list(s1, s2), function(i)
      any(i$Obj_var > i$Obj_mean)))) {
      alpha <- 0.1
    }
    
    # Return T/F if the tested difference in quantities is negative and statistically significant
    # T = s2 dominates s1
    
    indx <-
      setdiff(colnames(comp_df), c('soln_num', 'replication'))
    for (col in indx) {
      # if the column is to be maximized, multiply values by -1
      comp_df[[col]] <-
        comp_df[[col]] * sapply(
          X = .envir$optim_type,
          FUN = switch,
          'min' = 1,
          'max' = -1
        )[match(col, indx)]
    }
    if (.envir$stat_logical) {
      criteria_1 <- sapply(
        X = seq_along(indx),
        FUN = function(variable)
          #test: s1 - s2 = 0 i.e. s2 == s1
          t.test(formula = formula(paste0(
            indx[variable], ' ~ soln_num'
          )),
          data = comp_df)$p.value
      ) > alpha
      
      criteria_2 <- sapply(
        #s1 - s2 > 0  i.e. s2 < s1
        X = seq_along(indx),
        FUN = function(variable)
          t.test(
            formula = formula(paste0(indx[variable], ' ~ soln_num')),
            data = comp_df,
            alternative = 'g'
          )$p.value
      ) < alpha
      # Return T if the tested allocation's quantity is less (no statistical tests)
    } else {
      criteria_1 <-
        all(sapply(
          X = seq_along(indx),
          FUN = simple_mean_comparison,
          df = comp_df,
          strict = F
        ))
      criteria_2 <-
        any(sapply(
          X = seq_along(indx),
          FUN = simple_mean_comparison,
          df = comp_df,
          strict = T
        ))
    }
    return(all(mapply(sum, criteria_1, criteria_2)))
  }
