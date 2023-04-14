plotParetoFront <-
  function(inputData = NULL,
           plot_angle = 120,
           plot_replications = T,
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
      
      x <-
        rbindlist(lapply(
          inputData,
          FUN = function(i)
            i$Cost[,-1][, `:=`(sol = i$name,
                               distance = paste0('(',round(i$Divergence,digits = 2),',',i$P_Selection,')'))]
        ))[, metric := 'Replication Results']
      metric_names <- setdiff(colnames(x), c('metric', 'sol', 'distance'))
      x2 <-
        x[, lapply(.SD, mean), .SDcols = metric_names, by = sol
          ][, metric := 'Mean Response']
      idealPointDF <-
        data.table(t(matrix(
          apply(
            X = find_g_ideal(pSet = inputData, .envir = .envir),
            MARGIN = 2,
            FUN = mean
          )
        )))
      names(idealPointDF) <- metric_names
      idealPointDF <-
        cbind(
          idealPointDF,
          data.table(
            distance = NA_real_,
            metric = 'Mean Response',
            sol = 'Ideal Point'
          )
        )
      plotData <- rbind(x, x2,idealPointDF, fill = T,use.names = T)[, lapply(X = .SD,
                   FUN = as.numeric),
          .SDcols = metric_names,
          by = setdiff(colnames(x), metric_names)
          ][sol != 'Ideal Point', sol := paste0('Pareto_Solution_', as.numeric(as.factor(sol)))]
      
      idealPoint <- unlist(plotData[sol == 'Ideal Point',..metric_names])
      plotData <-
        rbind(plotData,
              Reduce(merge, lapply(metric_names, function(col)
                plotData[metric == 'Mean Response' & sol != 'Ideal Point', lapply(.SD, function(i, j = col)
                  (i + idealPoint[j]) / 2), .SDcols = col, by = sol]))[, metric := 'midpoint'],
              use.names = T,
              fill = T)[,distance := na.omit(unique(distance)),by = sol]
      
      ggplot() +
        geom_point(
          data = plotData[metric != 'midpoint'],
          aes_string(
            x = metric_names[1],
            y = metric_names[2],
            color = 'sol',
            alpha = 'metric',
            shape = 'metric',
            size = 'metric'
          )
        ) +
        scale_alpha_discrete(range = c(1, 0.33)) +
        scale_size_discrete(range = c(3, 1.5)) +
        annotate(
          geom = 'segment',
          x = plotData[metric == 'Mean Response', ][[metric_names[1]]],
          y = plotData[metric == 'Mean Response', ][[metric_names[2]]],
          xend = plotData[sol == 'Ideal Point', ][[metric_names[1]]],
          yend = plotData[sol == 'Ideal Point', ][[metric_names[2]]],
          alpha = 0.1
        ) +
        geom_text_repel(
          data = plotData[metric == 'midpoint'],
          position = 'jitter',
          aes_string(
            x = metric_names[1],
            y = metric_names[2],
            label = 'distance',
            colour = 'sol'
          ),fontface = 'bold',check_overlap = T) + 
        labs(x = str_to_title(gsub("_", " ", metric_names[1])), 
             y = str_to_title(gsub("_", " ", metric_names[2]))) +
        ggtitle('DB-PSA Pareto Image')

    } else if (length(.envir$optim_type) == 3) {
      if (any(class(inputData) == 'data.table')) {
        inputData <- as.matrix(inputData)
        paretoPlot <- scatterplot3d(inputData, grid = F)
        addgrids3d(inputData, grid = c("xy", "xz", "yz"))
        paretoPlot$points3d(inputData, pch = 16, type = 'h')
      } else if (class(inputData) == 'list') {
        idealPointDF <-
          data.table(t(matrix(
            apply(
              X = find_g_ideal(pSet = inputData, .envir = .envir),
              MARGIN = 2,
              FUN = mean
            )
          )),'Ideal Point')
        inputData2 <-
          rbindlist(lapply(
            inputData,
            FUN = function(i)
              i$Cost[, -1][,`:=`(sol = i$name,distance = round(i$Divergence,digits = 3),Probability = round(i$P_Selection,digits = 3))]
          ))
        metric_names <- setdiff(colnames(inputData2),c('sol','distance','Probability'))
        inputData <-
          inputData2[, lapply(X = .SD, FUN = mean), .SDcols = metric_names, by = sol]
        
        colnames(idealPointDF) <- setdiff(colnames(inputData2),c('distance','Probability'))
        #inputData <- rbind(inputData,idealPointDF,use.names = T)
        
        # Defining colors assocuated with each solution and the Ideal point
        n_sols <- inputData[,.N]
        colors <-
          palette(value = hcl.colors(n = n_sols,
                                     palette = 'Dark 2'))[sample(seq(n_sols), n_sols, replace = F)]
        color_inputData <-
          colors[as.numeric(as.factor(inputData[, sol]))]
        names(color_inputData) <- inputData[,sol]
        data_groups <- inputData2[, sol]
        inputData2 <- inputData2[,colour := color_inputData[match(sol,names(color_inputData))]]
        inputData <- inputData[inputData2[,lapply(.SD,unique),.SDcols = c('distance','Probability','colour'),by = 'sol'],on = .(sol)]
        rep_points_colors <- inputData2[,colour]
        names(rep_points_colors) <- inputData2$sol
        inputData2 <- inputData2[,colour :=paste0(colour,'99')]
        axis_labels <-
          str_to_title(gsub(
            x = colnames(inputData2)[1:3],
            pattern = '_',
            replacement = ' '
          ))
        x_col <- metric_names[1]
        y_col <- metric_names[2]
        z_col <- metric_names[3]
        
        if (plot_replications) {
          paretoPlot <-
            scatterplot3d(
              x = inputData2[, sol := NULL][, ..x_col] %>% unlist(),
              y = inputData2[, sol := NULL][, ..y_col] %>% unlist(),
              z = inputData2[, sol := NULL][, ..z_col] %>% unlist(),
              grid = F,
              pch = 3,
              color = inputData2[, colour] %>% unlist(),
              angle = plot_angle,
              main = 'Pareto Set Objective Metrics',
              xlab = axis_labels[1],
              ylab = axis_labels[2],
              zlab = axis_labels[3],
              highlight.3d = F
            )
          addgrids3d(inputData2,
                     grid = c("xy", "xz", "yz"),
                     angle = plot_angle)
          
          paretoPlot$points3d(
            copy(inputData)[, `:=`(sol = NULL, colour = NULL)],
            pch = 16,
            type = 'h',
            col = color_inputData,
            cex = 1.25
          )
        } else {
          paretoPlot <-
            scatterplot3d(
              x = unlist(inputData[, sol := NULL][, ..x_col]),
              y = unlist(inputData[, sol := NULL][, ..y_col]),
              z = unlist(inputData[, sol := NULL][, ..z_col]),
              grid = F,
              pch = 16,
              color = unlist(inputData[, colour]),
              angle = plot_angle,
              main = 'Pareto Set Objective Metrics',
              xlab = axis_labels[1],
              ylab = axis_labels[2],
              zlab = axis_labels[3],
              type = 'h',
              cex.symbols = 1.25
            )
          addgrids3d(inputData,
                     grid = c("xy", "xz", "yz"),
                     angle = plot_angle)
        }
        
        paretoPlot$points3d(idealPointDF[,`:=`(sol = NULL)],pch = 16,type = 'h',color = 'black',cex = 1.25)
        id_pt <- paretoPlot$xyz.convert(idealPointDF[1,])
        for(i in seq(inputData[,.N])){
          assign(paste0('p',i),copy(inputData)[,`:=`(sol = NULL)][i,])
          eval(parse(text = paste0('paretoPlot$points3d(rbind(copy(p',i,')[,colour := NULL],copy(idealPointDF),fill = T)[,`:=`(sol = NULL,colour = NULL)],type = "l",lty = 2,col = p',i,'[,colour])')))
          # eval(parse(text = paste0("label <- paretoPlot$xyz.convert(rbind(p",i,",copy(idealPointDF)[,`:=`(sol = NULL)],fill = T)[,lapply(.SD,mean),.SDcols = colnames(idealPointDF)])")))
          eval(parse(text = paste0("label <- paretoPlot$xyz.convert(p",i,")")))
          eval(parse(text = paste0("text(label$x,label$y,labels = paste0('(',p",i,"[,distance],',',p",i,"[,Probability],')'),pos = 3,font = 2,cex = 0.85, col = p",i,"[,colour])")))
        }
      }
    } else if (length(.envir$optim_type) > 3) {
      # To Do: pairwise scatterplot
    }
  }