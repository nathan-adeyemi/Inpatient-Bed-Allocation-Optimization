pareto_perc_correct <-
  function(i) {
    i <- conv_pSet_to_table(i)
    100 * (merge(
      x = i,
      y = true_pareto_set,
      by = colnames(true_pareto_set)
    )[, .N] /
      true_pareto_set[, .N])
  }
