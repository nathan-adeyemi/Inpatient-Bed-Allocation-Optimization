procedureI_func <- function(Object,all_candidates,.envir = parent.frame()) {  # OCBA Procedure I function
  Object$Psi <- unname(sum(sapply(all_candidates,
                                  function(i)
                                    ifelse(
                                      test = !identical(i, Object),
                                      yes = psiPVal(
                                        main = Object,
                                        indexed = i,
                                        .envir = .envir
                                      ),
                                      no = 0
                                    ))))
  
  return(Object)
} 