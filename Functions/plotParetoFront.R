plotParetoFront <-
  function(inputData = NULL,
           plot_angle = 120,
           .envir = parent.frame()) {
    # Function to plot the image of the Pareto front along the specified objective functions.
    
    # Plots a bold large symbol for a solutions expected objective function value as well as
    # smaller lighter symbols indicating the response of a single simulation replication
    
    # For a bi-objective problems, this function also plots the ideal point (a hypothetical point which dominates the entire Pareto set)
    # and the statistical distance between each solution in the Pareto set and this
    # ideal point
    
    # For tri-objective problems this function plots the Pareto image in 3 dimensional
    # euclidean space
    
    # For any problems with > 3 objectives, the function plots an N x N grid of pairwise scatter plots (N = number of objectives)
    
    # Inputs:
    #   inputData: a list of solutions in the Pareto set
    #   plot_angle: angle at which the 3D-space is shown for tri-objective problems
    
    inputData <-
      `if`(length(inputData) == 0, .envir$pareto_set, inputData)
    if (length(.envir$optim_type) == 2) {
      browser()
    } else if (length(.envir$optim_type) == 3) {
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
                                     palette = 'Dynamic'))[sample(seq(n_sols), n_sols, replace = F)]
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
            color = alpha(color_inputData2, .5),
            angle = plot_angle,
            main = 'Pareto Set Objective Metrics',
            xlab = axis_labels[1],
            ylab = axis_labels[2],
            zlab = axis_labels[3]
          )
        addgrids3d(inputData2,
                   grid = c("xy", "xz", "yz"),
                   angle = plot_angle)
        paretoPlot$points3d(
          inputData,
          pch = 16,
          type = 'h',
          col = color_inputData,
          cex = 1.25
        )
      }
    } else if (length(.envir$optim_type) > 3) {
      # To Do: pairwise scatterplot
    }
  }