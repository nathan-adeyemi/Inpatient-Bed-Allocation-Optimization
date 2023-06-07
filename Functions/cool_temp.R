cool_temp  <-
  function(initial_temperature = NA,
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
    }
    return(temp)
  }