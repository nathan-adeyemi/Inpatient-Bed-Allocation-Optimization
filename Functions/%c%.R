`%c%` <- function(x, n) {
  # helper function to find list elements with the same name
  sapply(x, `[[`, n)
}