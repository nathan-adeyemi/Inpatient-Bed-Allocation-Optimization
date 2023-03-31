cool_temp  <-
  function(initial_temperature = NA,
<<<<<<< HEAD
<<<<<<< HEAD
           alpha_param = NA,
           current_iteration = NA,
           cool_sched = NA,
           .envir = parent.frame()) {
    
    initial_temperature <- `if`(is.na(initial_temperature),.envir$temp_init,initial_temperature)
    alpha_param <- `if`(is.na(alpha_param),.envir$t_damp,alpha_param)
    current_iteration <- `if`(is.na(current_iteration),.envir$it,current_iteration)
    cool_sched <- `if`(is.na(cool_sched),'exponetial',cool_sched)
    if (grepl(x = cool_sched, pattern = 'linear|l')) {
      temp <-  initial_temperature / (1 + (alpha_param * current_iteration))
    } else if (grepl(x = cool_sched, pattern = 'log_mult|r')) {
      temp <-  initial_temperature / (1 + alpha_param * (log(1 + current_iteration)))
    } else if (grepl(x = cool_sched, pattern = 'quadratic|q')) {
      temp <- initial_temperature / (1 + (alpha_param * current_iteration ^ 2))
    } else if(grepl(x = cool_sched, pattern = 'exponential|e')){
      alpha_param <- sl_ifelse(test = alpha_param > 0,
                         yes = alpha_param * -1,
                         no = alpha_param)
      temp <-  initial_temperature * current_iteration ^ alpha_param
=======
           alpha = NA,
=======
           alpha_param = NA,
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
           current_iteration = NA,
           cool_sched = NA,
           .envir = parent.frame()) {
    
    initial_temperature <- `if`(is.na(initial_temperature),.envir$temp_init,initial_temperature)
<<<<<<< HEAD
    alpha <- ifelse(test = is.na(alpha), yes = .envir$t_damp, no = alpha)
    current_iteration <- ifelse(test = is.na(current_iteration),yes = .envir$it, no = current_iteration)
    
    if (linear) {
      temp <-  initial_temperature / (1 + (alpha * current_iteration))
    } else if (log_mult) {
      temp <-  initial_temperature / (1 + alpha * (log(1 + current_iteration)))
    } else if (quad_cool) {
      temp <- initial_temperature / (1 + (alpha * current_iteration ^ 2))
    } else if(exponential){
      alpha <- sl_ifelse(test = alpha > 0,
                         yes = alpha * -1,
                         no = alpha)
      temp <-  initial_temperature * current_iteration ^ alpha
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
    alpha_param <- `if`(is.na(alpha_param),.envir$t_damp,alpha_param)
    current_iteration <- `if`(is.na(current_iteration),.envir$it,current_iteration)
    cool_sched <- `if`(is.na(cool_sched),'exponetial',cool_sched)
    if (cool_sched == 'linear') {
      temp <-  initial_temperature / (1 + (alpha_param * current_iteration))
    } else if (cool_sched == 'log_mult') {
      temp <-  initial_temperature / (1 + alpha_param * (log(1 + current_iteration)))
    } else if (cool_sched == 'quadratic') {
      temp <- initial_temperature / (1 + (alpha_param * current_iteration ^ 2))
    } else if(cool_sched == 'exponential'){
      alpha_param <- sl_ifelse(test = alpha_param > 0,
                         yes = alpha_param * -1,
                         no = alpha_param)
      temp <-  initial_temperature * current_iteration ^ alpha_param
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
    }
    return(temp)
  }