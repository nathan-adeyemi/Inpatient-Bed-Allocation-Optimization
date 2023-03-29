p_accept <-
<<<<<<< HEAD
<<<<<<< HEAD
  function(curr_temp = NA,
           initial_temp = NA,
           .envir = parent.frame()) {
=======
  function(curr_temp, initial_temp) {
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
  function(curr_temp = NA,
           initial_temp = NA,
           .envir = parent.frame()) {
>>>>>>> 8c8946d (Fixed some custom functions.)
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
