extractHalfWidth <-
  function(ci) {
    hw <- diff(as.numeric(gsub(
      pattern = "\\(|\\)",
      replacement = "",
      x = unlist(strsplit(x = ci, split = ','))
    ))) / 2
    return(hw)
  }