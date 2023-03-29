plotTestBenchResults <- function(df,.envir = parent.frame()){
  df_avgs <-
    setDT(
      melt(
        data = copy(df)[, `:=`(
          mean_server_utilization = (100 * mean_server_utilization / .SD[Iteration == 0, mean_server_utilization]) -
            100,
          max_mean_queue = (100 * max_mean_queue / .SD[Iteration == 0, max_mean_queue]) - 100,
          avg_wait = (100 * avg_wait / .SD[Iteration == 0, avg_wait]) - 100 ,
          Dist = signif(Dist, digits = 4)
        ), by = instance], 
        measure.vars = colnames(best$Obj_mean)
      )
    )[,.(value = mean(value),sd = sd(value)),by = list(Iteration,variable)]
  
  p <- ggplot(data = df_avgs,
              mapping = aes(x = Iteration, y = value, colour = variable)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = value - sd,
                      ymax = value + sd),
                  width = .2,
                  position = position_dodge(0.05))
  return(p)
}