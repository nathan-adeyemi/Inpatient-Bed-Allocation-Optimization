fix_cell <- function(i){
  if (i < 0){
    i <- abs(i)
  } else if (i > 1){
    i <- (1-i) + 1
  }
  i
}
