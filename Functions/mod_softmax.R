mod_softmax <- function(vec) {
  vec <- scale(vec)
  return(exp(vec) / sum(exp(vec)))
}