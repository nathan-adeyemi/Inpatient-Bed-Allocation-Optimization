conv_pSet_to_table <- function(set){
  if(length(set) == 1){
    set = set[[1]]
  }
  data.table(t(set %c% 'Allocation'))
}