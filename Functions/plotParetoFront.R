plotParetoFront <-
  function(inputData = NULL,
           plot_angle = 120,
           plot_replications = T,
           plot_ideal_point = T,
           plot_initial_point = F,
           scatter_matrix = F,
           orig_alloc_path,
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
    #   inputData: (List) a list of solutions in the Pareto set
    #   plot_angle: (Interger) angle at which the 3D-space is shown for tri-objective problems
    #   plot_replications: (Logical) Should points for individual replications be included
    #   plot_ideal_point: (Logical) Should the ideal point be included in the scatterplot
    #   plot_initial_point: (Logical) Should the objective metrics of the current bed allocation be plotted
    #   scatter_matrix: (Logical) T -> return a pairwise matrix of 2D scatterplots
    
    inputData <-
      `if`(length(inputData) == 0, .envir$pareto_set, inputData)
    
    if (is.list(inputData)) {
      metric_names <- names(inputData[[1]]$Obj_mean)
    }
    if(length(.envir$optim_type) > 3){
      scatter_matrix <-  T
    }
    if (length(.envir$optim_type) == 2) {
      if(any(grepl('Cost',names(inputData[[1]])))){
        if (plot_ideal_point) {
          plotData <-
            rbindlist(lapply(
              inputData,
              FUN = function(i)
                i$Cost[, -1][,sol := i$name][, lapply(.SD, mean), .SDcols = metric_names, by = sol][, `:=`(distance = paste0('(', round(i$Divergence, digits = 2), ',', i$P_Selection, ')'))]
            ))[, `:=`(metric = 'Mean Response',sol = paste0('Pareto \n Solution ', as.numeric(as.factor(sol))))]
        } else{
          plotData <-
            rbindlist(lapply(
              inputData,
              FUN = function(i)
                i$Cost[, -1][, sol := i$name]
            ))[, lapply(.SD, mean), .SDcols = metric_names, by = sol][, `:=`(metric = 'Mean Response',
                                                                             sol = paste0('Pareto Solution ', as.numeric(as.factor(sol))))]
        }
      } else {
        plotData <-
          rbindlist(lapply(
            inputData,
            FUN = function(i) data.table(pivot_wider(enframe(i$Obj_mean),names_from = 'name'))[,sol := i$name]
          ))[, `:=`(metric = 'Mean Response',sol = paste0('Pareto Solution ', as.numeric(as.factor(sol))))]
      }
      if(plot_ideal_point){
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
      }
      if(plot_replications){
        replicationData <-
          rbindlist(lapply(
            inputData,
            FUN = function(i)
              i$Cost[,-1][, `:=`(sol = i$name,
                                 distance = paste0('(',round(i$Divergence,digits = 2),',',i$P_Selection,')'))]
          ))[, metric := 'Replication Results']
        plotData <- rbind(replicationData, plotData,fill = T,use.names = T
                          )[, sol := paste0('Pareto Solution ', as.numeric(as.factor(sol)))]
      }
      if(plot_ideal_point){
        rbind(plotData,idealPointDF, fill = T,use.names = T)
        idealPoint <- unlist(idealPointDF[sol == 'Ideal Point',..metric_names])
      }
      plotData <- plotData[, lapply(X = .SD,
                   FUN = as.numeric),
          .SDcols = metric_names,
          by = setdiff(colnames(plotData), metric_names)]
      if(plot_ideal_point){
        plotData <-
          rbind(rbind(plotData,
                Reduce(merge, lapply(metric_names, function(col)
                  plotData[metric == 'Mean Response' &
                             sol != 'Ideal Point', lapply(.SD, function(i, j = col)
                               (i + idealPoint[j]) / 2), .SDcols = col, by = sol]))[, metric := 'midpoint'],
                use.names = T,
                fill = T)[, distance := na.omit(unique(distance)), by = sol],idealPointDF,fill = T)
      }
      plt <- ggplot() +
        geom_point(
          data = plotData[metric != 'midpoint'],
          aes_string(
            x = metric_names[1],
            y = metric_names[2],
            color = 'sol',
            alpha = 'metric',
            shape = 'metric'
          ),
          size = 4
        ) +
        geom_text(
          data = plotData[metric != 'midpoint'],
          aes_string(x = metric_names[1],
                     y = metric_names[2],
                     label = 'sol',
                     color = 'sol'),
          nudge_y = 0.1,
          size = 2.75,
          fontface = 'bold'
        ) +
        scale_alpha_discrete(range = c(1, 0.33)) +
        scale_size_discrete(range = c(3, 1.5)) +
        labs(x = str_to_title(gsub("_", " ", metric_names[1])),
             y = str_to_title(gsub("_", " ", metric_names[2]))) +
        ggtitle('DD-PUSA Estimated Pareto Front')
      
      if (plot_ideal_point) {
        plt <- plt + annotate(
          geom = 'segment',
          x = plotData[metric == 'Mean Response',][[metric_names[1]]],
          y = plotData[metric == 'Mean Response',][[metric_names[2]]],
          xend = plotData[sol == 'Ideal Point',][[metric_names[1]]],
          yend = plotData[sol == 'Ideal Point',][[metric_names[2]]],
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
            ),
            fontface = 'bold',
            check_overlap = T,
            size = 3.5) + 
          xlab('Objective #1') + 
          ylab('Objective #2') + 
          theme(panel.background = element_rect(fill = "grey90"),legend.position = 'none')
      
      }
      return(plt)
    } else if (length(.envir$optim_type) == 3 & !scatter_matrix) {
      if (any(class(inputData) == 'data.table')) {
        inputData <- as.matrix(inputData)
        paretoPlot <- scatterplot3d(inputData, grid = F)
        addgrids3d(inputData, grid = c("xy", "xz", "yz"))
        paretoPlot$points3d(inputData, pch = 16, type = 'h')
      } else if (class(inputData) == 'list') {
        plotData <-
          rbindlist(lapply(
            inputData,
            FUN = function(i)
              i$Cost[, -1][,`:=`(sol = i$name)])
            )[,sol := paste0('Pareto Set Solution ',as.numeric(as.factor(sol)))
              ][, lapply(X = .SD, FUN = mean), .SDcols = metric_names, by = sol]
        
        if(plot_replications){
          replicationData <-
            rbindlist(lapply(
              inputData,
              FUN = function(i)
                i$Cost[, -1][,`:=`(sol = i$name,distance = round(i$Divergence,digits = 3),Probability = round(i$P_Selection,digits = 3))]
            ))[,sol := paste0('Pareto Set Solution ',as.numeric(as.factor(sol)))]
        }
        
        if(plot_initial_point){
          initial_point <- readRDS(.envir$orig_alloc_path)
          if(is.list(initial_point)){
            initial_point <- initial_point[[1]]
          }
          initial_point <- objective_Metrics(initial_point,.envir = .envir)
          #browser()
          initial_point_means <- initial_point[,lapply(.SD,mean),.SDcols = setdiff(colnames(initial_point),'replication')]
          plotData <- rbind(plotData,data.table(sol = 'Baseline Simulation',initial_point_means),fill = T)
          # initial_point_coords <- paretoPlot$xyz.convert(initial_point_means)
          # paretoPlot$points3d(initial_point_means,
          #                     pch = 16,
          #                     type = 'h',
          #                     color = 'black',
          #                     cex = 1.25)
          # text(initial_point_coords$x,initial_point_coords$y,labels = "Current Bed \n Allocation",pos = 4, offset  = 1,cex = 0.7)
        }
        
        if(plot_ideal_point){
          idealPointDF <-
            data.table(t(matrix(
              apply(
                X = find_g_ideal(pSet = inputData, .envir = .envir),
                MARGIN = 2,
                FUN = mean
              )
            )),'Ideal Point')
          colnames(idealPointDF) <- setdiff(colnames(replicationData),c('distance','Probability'))
        }
        # Defining colors assocuated with each solution and the Ideal point
        n_sols <- plotData[,.N]
        colors <-
          palette(value = hcl.colors(n = n_sols,
                                     palette = 'Dark 2'))[sample(seq(n_sols), n_sols, replace = F)]
        plotData <- plotData[,`:=`(colour = colors[as.numeric(as.factor(sol))],
                                   pch = 16,
                                   type = 'h',
                                   cex = 1.25)]
        if(plot_initial_point){
          plotData[sol == 'Baseline Simulation',`:=`(colour = 'black',
                                                     cex = 5,
                                                     pch = 8,
                                                     type = 's')]
        }
        color_inputData <-plotData[,colour]
        names(color_inputData) <- legend_groups <- plotData[,sol]
        legend_groups <- as.factor(legend_groups)
        data_groups <- plotData[, sol]
        axis_labels <-
          str_to_title(gsub(
            x = colnames(copy(plotData)[,sol:= NULL]),
            pattern = '_',
            replacement = ' '
          ))
        x_col <- metric_names[1]
        y_col <- metric_names[2]
        z_col <- metric_names[3]
        if (plot_replications) {
          replicationData <- replicationData[,colour := color_inputData[match(sol,names(color_inputData))]]
          plotData <- plotData[replicationData[,lapply(.SD,unique),.SDcols = c('distance','Probability','colour'),by = 'sol'],on = .(sol)]
          rep_points_colors <- replicationData[,colour]
          names(rep_points_colors) <- replicationData$sol
          replicationData <- replicationData[,colour :=paste0(colour,'99')]
          paretoPlot <-
            scatterplot3d(
              x = replicationData[, sol := NULL][, ..x_col] %>% unlist(),
              y = replicationData[, sol := NULL][, ..y_col] %>% unlist(),
              z = replicationData[, sol := NULL][, ..z_col] %>% unlist(),
              grid = F,
              pch = 3,
              color = replicationData[, colour] %>% unlist(),
              angle = plot_angle,
              main = 'Pareto Set Objective Metrics',
              xlab = axis_labels[1],
              ylab = axis_labels[2],
              zlab = axis_labels[3],
              highlight.3d = F
            )
          addgrids3d(replicationData,
                     grid = c("xy", "xz", "yz"),
                     angle = plot_angle)
          
          paretoPlot$points3d(
            copy(plotData)[, `:=`(sol = NULL, colour = NULL)],
            pch = 16,
            type = 'h',
            col = color_inputData,
            cex = 1.25
          )
        } else {
          paretoPlot <-
            scatterplot3d(
              x = unlist(plotData[, sol := NULL][, ..x_col]),
              y = unlist(plotData[, sol := NULL][, ..y_col]),
              z = unlist(plotData[, sol := NULL][, ..z_col]),
              grid = F,
              pch = unlist(plotData[,pch]),
              color = unlist(plotData[, colour]),
              angle = plot_angle,
              main = 'Pareto Set Objective Metrics',
              xlab = axis_labels[1],
              ylab = axis_labels[2],
              zlab = axis_labels[3],
              type = 'h',
              cex.symbols = 1.25
            )
          addgrids3d(plotData,
                     grid = c("xy", "xz", "yz"),
                     angle = plot_angle)
        }
        if (plot_ideal_point) {
          paretoPlot$points3d(
            idealPointDF[, `:=`(sol = NULL)],
            pch = 16,
            type = 'h',
            color = 'black',
            cex = 1.25
          )
          id_pt <- paretoPlot$xyz.convert(idealPointDF[1, ])
          for (i in seq(plotData[, .N])) {
            assign(paste0('p', i), copy(plotData)[, `:=`(sol = NULL)][i, ])
            eval(parse(
              text = paste0(
                'paretoPlot$points3d(rbind(copy(p',
                i,
                ')[,colour := NULL],copy(idealPointDF),fill = T)[,`:=`(sol = NULL,colour = NULL)],type = "l",lty = 2,col = p',
                i,
                '[,colour])'
              )
            ))
            eval(parse(text = paste0(
              "label <- paretoPlot$xyz.convert(p", i, ")"
            )))
            eval(parse(
              text = paste0(
                "text(label$x,label$y,labels = paste0('(',p",
                i,
                "[,distance],',',p",
                i,
                "[,Probability],')'),pos = 3,font = 2,cex = 0.85, col = p",
                i,
                "[,colour])"
              )
            ))
          }
        }
        
        #legend('right', col = color_inputData[order(legend_groups)] ,bg="white",pch = 16, yjust=0, legend = sort(legend_groups), cex = 0.75)
      }
    } else if (scatter_matrix) {
      # To Do: pairwise scatterplot
      idealPointDF <-
        data.table(t(matrix(
          apply(
            X = find_g_ideal(pSet = inputData, .envir = .envir),
            MARGIN = 2,
            FUN = mean
          )
        )),'Ideal Point')
      replicationData <-
        rbindlist(lapply(
          inputData,
          FUN = function(i)
            i$Cost[, -1][,`:=`(sol = i$name,distance = round(i$Divergence,digits = 3),Probability = round(i$P_Selection,digits = 3))]
        ))[,sol := paste0('Pareto Set Solution ',as.numeric(as.factor(sol)))]
      metric_names <- setdiff(colnames(replicationData),c('sol','distance','Probability'))
      inputData <-
        replicationData[, lapply(X = .SD, FUN = mean), .SDcols = metric_names, by = sol]
      
      colnames(idealPointDF) <- setdiff(colnames(replicationData),c('distance','Probability'))
      #inputData <- rbind(inputData,idealPointDF,use.names = T)
      
      # Defining colors assocuated with each solution and the Ideal point
      n_sols <- inputData[,.N]
      colors <-
        palette(value = hcl.colors(n = n_sols,
                                   palette = 'Dark 2'))[sample(seq(n_sols), n_sols, replace = F)]
      color_inputData <-
        colors[as.numeric(as.factor(inputData[, sol]))]
      names(color_inputData) <- legend_groups <- inputData[,sol]
      legend_groups <- as.factor(legend_groups)
      data_groups <- replicationData[, sol]
      replicationData <- replicationData[,`:=`(colour = color_inputData[match(sol,names(color_inputData))])]
      inputData <-
        inputData[replicationData[, lapply(X = .SD, FUN = unique), .SDcols = c('distance', 'Probability', 'colour'), by = 'sol'], on = .(sol)
                  ][,`:=`(point_type = 16,cex = 1.25)]
      rep_points_colors <- replicationData[,colour]
      names(rep_points_colors) <- replicationData$sol
      replicationData <- replicationData[,colour :=paste0(colour,'99')]
      
      if(plot_ideal_point){
        inputData <- rbindlist(list(inputData,idealPointDF),fill = T)[sol == 'Ideal Point',`:=`(colour = '#000000',cex = 1.25,point_type = 16)]
      }
      
      if(plot_initial_point){
        initial_point <- readRDS(.envir$orig_alloc_path)
        initial_point <- initial_point[[1]]
        initial_point <- objective_Metrics(initial_point,.envir = .envir)
        inputData <-
          rbindlist(list(
            inputData,
            data.table(
              initial_point[, lapply(.SD, mean), .SDcols = setdiff(colnames(initial_point), 'replication')],
              colour = 'black',
              point_type = 16,
              sol = 'Original Allocation'
            )
          ), fill = T)
        if(plot_replications){
          replicationData <- 
            rbindlist(list(
              replicationData,
              data.table(
                initial_point,
                colour = "#00000099",
                sol = 'Original Allocation')
            ), fill = T)[, replication := NULL]
        }
      }

      if(plot_replications){
        inputData <- rbindlist(list(inputData,replicationData),fill = T)[,`:=`(cex = 0.75,point_type = 8)]
      }
      
      with(inputData,
           pairs(
             formula(paste("~", paste(
               metric_names, collapse = ' + '
             ))),
             col = colour,
             pch = point_type,
             labels = gsub(
               pattern = 'V1',
               replacement = 'Increase in /n total Throughput',
               x = stringr::str_to_title(gsub("_", " ", metric_names))
             )
           ))
    }
  }

