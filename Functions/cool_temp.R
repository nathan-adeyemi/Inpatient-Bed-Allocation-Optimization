cool_temp  <-
  function(initial_temperature = NA,
           alpha = NA,
           current_iteration = NA,
           exponential = F,
           linear = F,
           log_mult = F,
           quad_cool = F,
           constant = F, 
           .envir = parent.frame()) {
    
    initial_temperature <- `if`(is.na(initial_temperature),.envir$temp_init,initial_temperature)
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
    }
    return(temp)
  }