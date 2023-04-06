reorder_pSet <- function(pSet,.envir = parent.frame()) {
  pareto_order <-
    data.table(name = pSet %c% 'name', t(pSet %c% 'Obj_mean'))[, .(apply(.SD, 2, unlist))]
  pareto_order <-
    pareto_order[, setdiff(colnames(pareto_order), 'name') := lapply(.SD, as.numeric), .SDcols = setdiff(colnames(pareto_order), 'name')]
  pareto_order <-
    pareto_order[, (setdiff(colnames(pareto_order), 'name')[grep('min', .envir$optim_type)]) := lapply(
      X = .SD,
      FUN = function(i)
        i * -1
    ), .SDcols = setdiff(colnames(pareto_order), 'name')[grep('min', .envir$optim_type)], by = name]
  setkeyv(pareto_order, setdiff(colnames(pareto_order), 'name'))
  pSet <-
    pSet[match(pareto_order$name, pSet %c% 'name')]
  return(pSet)
}