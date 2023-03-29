removeDuplicateSolutions <- function(front){
  return(front[!duplicated(lapply(
    X = front,
    FUN = function(i) i$name))])
}
