specify_decimal <-
  function(x, digits) {
    x <- as.numeric(x = trimws(x = format(
      x = round(x = x, digits = digits), nsmall = digits
    )))
    return(x)
  }