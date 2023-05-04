generateBoxplots <-
  function(inputData,
           plotIdealPoint = F,
           .envir = parent.frame()) {
    initial_point <- readRDS(.envir$origin_alloc_path)[[1]]
    initial_point <-
      objective_Metrics(initial_point, .envir = .envir)[, replication := NULL]
    if (plotIdealPoint) {
      idealPointDF <- data.table(find_g_ideal(inputData, .envir = .envir))
      # initial_point_means <- initial_point[,lapply(.SD,mean),.SDcols = setdiff(colnames(initial_point),'replication')]
      names(idealPointDF) <- names(initial_point)
      plotData <-
        melt(rbind(idealPointDF[, name := 'Ideal Point'], initial_point[, name := 'Original Bed Allocation']))
      ggplot(data = plotData, aes(value,fill = name)) + 
        geom_boxplot(position = 'dodge2') + 
        facet_wrap(facets = ~variable, scales = 'free') + 
        coord_flip() + 
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom') 
    } else {
      # Generates boxplots of each Pareto set solution next to the original bed allocation
      plotData_full <-
        melt(rbind(rbindlist(lapply(
          X = seq_along(inputData),
          FUN = function(i)
            inputData[[i]]$Cost[, `:=`(Solution = paste0('Solution ', i),
                                       replication = NULL)]
        )),
        initial_point[, Solution := 'Original Bed Allocation']))
      
      plotList <- lapply(
        #X = seq_along(inputData),
        X = seq_along(inputData),
        FUN = function(i) {
          plotData <-
            plotData_full[(Solution == paste0('Solution ', i) |
                            Solution == 'Original Bed Allocation')]
          p <- ggplot(data = plotData, aes(value,Solution)) +
            geom_boxplot(aes(fill = Solution)) +
            facet_wrap(
              facets = ~ variable,
              scales = 'free_y',
              strip.position = "top"
            ) +
            invisible(coord_cartesian(ylim = c(min(boxplot.stats(plotData$value)$stats), max(boxplot.stats(plotData$value)$stats)))) +
            coord_flip() +
            theme(
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.position = 'none',
              strip.background = `if`(i > 1,element_blank(),element_rect(fill = 'grey')),
              strip.text = `if`(i>1,element_blank(),element_text(face = 'bold',color = 'black')))
          })
      gridExtra::grid.arrange(grobs = plotList,ncol = 1)
    }
}