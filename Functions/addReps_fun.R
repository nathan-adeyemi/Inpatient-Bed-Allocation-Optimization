addReps_fun <-
  function(p,
           candidate_list,
           k_test_val,
           deltaRef,
           reference_Psi,
           .envir = parent.frame()) {
    if (p == k_test_val){
      deltaRef
    } else if (p < k_test_val){
      candidate_list[[p]]$deltaPsiD/reference_Psi * deltaRef
    } else {
      0
    }
  }
