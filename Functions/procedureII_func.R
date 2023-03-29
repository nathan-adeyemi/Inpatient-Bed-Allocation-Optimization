procedureII_func <-
  function(Object,
           kVal,
           all_candidates,
           include_All_D = T,
           .envir = parent.frame()) {
    # OCBA Procedure II Function
    # Arg include_All_D: True -> consider change in Psi for all other solutions
    #                    False -> consider change in Psi for just solution i
    sub_val_1 = sub_val_2 = 1
    psiID <- c()
    num_obj = ncol(Object$Obj_mean)
    if (include_All_D) {
      Object$psiID <- round(x = sapply(
        X = all_candidates,
        FUN = function(d) {
          if (!identical(Object$name, d$name)) {
            ret <-
              unname(obj = (
                psiPVal(Object, indexed = d, .envir = .envir) - psiPVal(
                  Object,
                  indexed = d,
                  hat = TRUE,
                  .envir = .envir
                )
              ))
          } else {
            # equation (7)
            ret <- sum(sapply(
              X = all_candidates,
              FUN = function(j)
                psiPVal(Object, indexed = j, .envir = .envir)
            )) - sum(sapply(
              X = all_candidates,
              FUN = function(j)
                psiPVal(
                  Object,
                  indexed = j,
                  self_hat = T,
                  .envir = .envir
                )
            ))
          }
          return(ret)
        }
      ),
      digits = 7)
      
    } else {
      ind <- which(Object$name == (all_candidates %c% 'name'))
      Object$psiID <-
        round(x = sum(sapply(
          X = all_candidates,
          FUN = function(j) {
            psiPVal(Object, indexed = j, .envir = .envir)
          }
        )) -
          sum(sapply(
            X = all_candidates,
            FUN = function(j) {
              psiPVal(
                Object,
                indexed = j,
                self_hat = T,
                .envir = .envir
              )
            }
          )), digits = 7)
    }
    return(Object)
  }