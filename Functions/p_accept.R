p_accept <-
  function(curr_temp, initial_temp) {
    curr_temp <-
      ifelse(test = is.na(curr_temp),
             yes = .envir$temp,
             no = curr_temp)
    initial_temp <-
      ifelse(
        test = is.na(initial_temp),
        yes = .envir$temp_init,
        no = initial_temp
      )
    
    return(1 - (exp(initial_temp - curr_temp) / exp(initial_temp)))
  }
