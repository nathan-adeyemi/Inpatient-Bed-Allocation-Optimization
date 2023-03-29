candidateSettoMatrix <- function(set, attr, .envir = parent.frame()) {
  attr <- if (!is.character(attr))
    deparse(substitute(attr))
  else
    attr
  return(t(matrix(unlist(set %c% attr), ncol = length(set))))
}