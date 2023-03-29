permute_with_duplicates <- function(v, n) {
  perms <- permutations(length(v), n, v, repeats.allowed = TRUE)
  return(perms)
}