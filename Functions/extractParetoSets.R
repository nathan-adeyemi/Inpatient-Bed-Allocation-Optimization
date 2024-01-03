extractParetoSets <- function(directory,
                              .envir = parent.frame()) {
  pareto_set <- lapply(
    X = seq(length(list.files(
      file.path(directory,
                "Inverted_V_Full_Environments")
    ))),
    FUN =  function(i) {
      load(file.path(
        directory,
        "Inverted_V_Full_Environments",
        paste0('Trial_', i, '.Rdata')
      ))
      return(pareto_set)
    }
  )
}
