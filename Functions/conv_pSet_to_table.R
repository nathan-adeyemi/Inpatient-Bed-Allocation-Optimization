conv_pSet_to_table <- function(set){
<<<<<<< HEAD
  if(length(set) == 1){
    set = set[[1]]
  }
  data.table(t(set %c% 'Allocation'))
=======
  data.table(t(set[[1]] %c% 'Allocation'))
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
}