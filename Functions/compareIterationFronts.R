compareIterationFronts <-
  function(curr_front, prev_front, .envir = parent.frame()) {
    return(
      identical(
        removeDuplicateSolutions(curr_front) %c% 'name',
        removeDuplicateSolutions(prev_front)  %c% 'name'
      )
    )
  }
