nsgaCostFn <- function(x) {
  return(CostFunction(
    sol = x,
    logic = F,
    nsga = T
  ))
}