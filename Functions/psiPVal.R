psiPVal <-
  function(main,
           indexed,
           hat = FALSE,
           self_hat = FALSE,
           .envir = parent.frame()) {
    #P(F_jindex <= F_iindex)
    delta <- .envir$delta
    x_bar = main$Obj_mean - indexed$Obj_mean
    if (self_hat) {
      sigma_est_body =  sqrt(
        main$Obj_var / (main$Replications + delta) + indexed$Obj_var / indexed$Replications
      )
    } else if (!hat) {
      sigma_est_body = sqrt(main$Obj_var / main$Replications + indexed$Obj_var /
                              indexed$Replications)
    } else{
      sigma_est_body = sqrt(
        indexed$Obj_var / (indexed$Replications + delta) + main$Obj_var / main$Replications
      )
    }
    p_indexed_better_main <- prod(
      sapply(
        X = colnames(x_bar),
        FUN = psiPval_subfunction,
        means = x_bar,
        deviations = sigma_est_body,
        .envir = .envir
      )
    )
    return(p_indexed_better_main)
  }