fix_optim_type <- function(x = NA, .envir = parent.frame()) {
  
  # Convenience Function: Replaces any "+" with "max" and any "-" with "min".
  #                       strings "max" and "min" are preferred in other functions
  
  # Inputs: x - (Character) Vector of optimization types (either "max/min" or "+/-")
  
  # Returns: (Character) Fixed optim type vector
  
  if (all(is.na(x))) {
    x <- .envir$optim_type
  }
  x <- sapply(x, gsub, pattern = "\\+", replacement = 'max')
  x <- sapply(x, gsub, pattern = "\\-", replacement = 'min')
  return(x)
}