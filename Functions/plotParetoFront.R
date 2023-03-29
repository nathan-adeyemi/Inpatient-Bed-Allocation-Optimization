plotParetoFront <-
  function(inputData = NULL,
           plot_angle = 120,
           .envir = parent.frame()) {
    inputData <- if(length(inputData) == 0) .envir$pareto_set else inputData
  if (any(class(inputData) == 'data.table')) {
    inputData <- as.matrix(inputData)
    paretoPlot <- scatterplot3d(inputData, grid = F)
    addgrids3d(inputData, grid = c("xy", "xz", "yz"))
    paretoPlot$points3d(inputData, pch = 16, type = 'h')
  } else if (class(inputData) == 'list') {
    inputData2 <-
      rbindlist(lapply(
        inputData,
        FUN = function(i)
          i$Cost[, -1][, sol := i$name]
      ))
    n_sols <- length(unique(inputData2$sol))
    colors <-
      palette(value = hcl.colors(n = n_sols, 
                                 palette = 'Dynamic'))[sample(seq(n_sols),n_sols,replace = F)]
    color_inputData <-
      colors[as.numeric(as.factor(inputData %c% 'name'))]
    inputData <-
      candidateSettoMatrix(set = inputData, attr = 'Obj_mean')
    color_inputData2 <-
      colors[as.numeric(as.factor(inputData2[, sol]))]
    data_groups <- inputData2[, sol]
    axis_labels <-
      str_to_title(gsub(
        x = colnames(inputData2)[1:3],
        pattern = '_',
        replacement = ' '
      ))
    paretoPlot <-
      scatterplot3d(
        copy(inputData2)[, sol := NULL],
        grid = F,
        pch = 8,
        color = alpha(color_inputData2,.5),
        angle = plot_angle,
        main = 'Pareto Set Objective Metrics',
        xlab = axis_labels[1],
        ylab = axis_labels[2],
        zlab = axis_labels[3]
      )
    addgrids3d(inputData2, grid = c("xy", "xz", "yz"),angle = plot_angle)
    paretoPlot$points3d(inputData,
                        pch = 16,
                        type = 'h',
                        col = color_inputData,
                        cex = 1.25)
  }
}