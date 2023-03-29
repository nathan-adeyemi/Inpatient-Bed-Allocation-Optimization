updateSimStats <- function(i, data, new_sol) {
  i$Cost <- if (!new_sol)
    rbind(i$Cost, data)
  else
    data
  i$Cost[, replication := seq(nrow(i$Cost))]
  setDT(i$Cost)
  i$Obj_mean <-
    i$Cost[, lapply(.SD, mean, na.rm = T), .SDcols = colnames(i$Cost)][, replication := NULL]
  i$Obj_var <-
    i$Cost[, lapply(.SD, sd, na.rm = T), .SDcols = colnames(i$Cost)][, replication := NULL]
  i$Obj_CI <-
    i$Cost[, lapply(.SD, function(i)
      ci_as_text(interval = t.test(i, conf.level = 0.95)$conf.int)), .SDcols = colnames(i$Cost)][, replication := NULL]
  i[['addReps']] <- NULL
  i[['deltaPsiD']] <- NULL
  i[['psiID']] <- NULL
  return(i)
}