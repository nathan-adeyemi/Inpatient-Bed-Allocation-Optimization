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
  return(perms)
}