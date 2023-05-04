<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
permute_with_duplicates <- function(v, n, orig = F) {
  perms <- as.matrix(t(restrictedparts(v, n)))
  perms <- mclapply(
    X = seq(nrow(perms)),
    FUN = function(row) {
      x = combinat::permn(x = perms[row,])
      x = x[!duplicated(x)]
      return(x)
    },
    mc.cores = availableCores()
  )
  perms <-
    data.table(matrix(
      data  = unlist(perms, recursive = T),
      ncol = n,
      byrow = T
    ))
<<<<<<< HEAD
=======
permute_with_duplicates <- function(v, n) {
  perms <- permutations(length(v), n, v, repeats.allowed = TRUE)
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
=======
>>>>>>> a420328 (Git Repo updates)
  return(perms)
}