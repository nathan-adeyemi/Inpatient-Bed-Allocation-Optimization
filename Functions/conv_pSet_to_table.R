conv_pSet_to_table <- function(set){
  data.table(t(set[[1]] %c% 'Allocation'))
}