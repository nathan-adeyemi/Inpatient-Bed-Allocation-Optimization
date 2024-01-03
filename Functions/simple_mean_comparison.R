simple_mean_comparison <- function(metric, df, strict = T) {
  if (strict)
    df[soln_num == 2, sapply(.SD, mean), .SDcols = metric] < df[soln_num == 1, sapply(.SD, mean), .SDcols = metric]
  else
    df[soln_num == 2, sapply(.SD, mean), .SDcols = metric] <= df[soln_num == 1, sapply(.SD, mean), .SDcols = metric]
}
