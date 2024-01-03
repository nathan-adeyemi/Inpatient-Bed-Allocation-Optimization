`%+%` <- function(x,y){
  eval.parent(substitute(x <- x + y))
}