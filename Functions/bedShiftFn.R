bedShiftFn <-
  function(sol,
           df = siteInfo,
           hosplist = readRDS(file.path('.','Data','plotting_utilities','hosplist.rds')),
           facility_locations = readRDS(file.path(".", "Data", "plotting_utilities", "coordinates.rds")),
           aliases = readRDS(file.path('.','Data','plotting_utilities','Hospital Aliases.rds')),
           counts_by_age = TRUE,
           total_displaced = FALSE,
           generate_plot = FALSE,
           top_plot = T){

    if(generate_plot){
      counts_by_age <- total_displaced <- FALSE
    }else if(total_displaced){
      counts_by_age <- generate_plot <- FALSE
    }
    
    setDT(hosplist)
    setDT(facility_locations)
    
    facility_locations <-
      facility_locations[, `:=`(lower_name = tolower(name))]
    facility_locations <- merge(facility_locations,aliases,by.x = 'lower_name',by.y = 'aliases')
    facility_locations <- unique(facility_locations[,list(Facility_name = name.y,lon,lat,county)])
    plotData <-
      df[!duplicated(Bed_Group), list(Facility_name, Bed_Group, total_beds)
                                      ][, new_allocation := sol
                                        ][, change := new_allocation - total_beds
                                          ][facility_locations, `:=`(county = county), on = c(Facility_name = 'Facility_name')]
    plotList_env <- environment()
    plotList <- lapply(
      X = c('Adult', 'Adolescent', 'Pediatric', 'Geriatric'),
      FUN = function(age,.envir = parent.frame()) {
        # browser()
        plotData_age <-
          plotData[grepl(pattern = age, x = Bed_Group),
                   ][, `:=`(subregion = tolower(gsub(pattern = ' County',
                                                     replacement = '',
                                                     x = county)))
                     ][, .(`Change in # of Licensed Beds` = sum(change)), by = subregion]
        if(.envir$counts_by_age){
          return(data.table(age,plotData_age[,sum(`Change in # of Licensed Beds`)]))
        }else{
        county_borders <- map_data('county', 'minnesota')
        test <-
          left_join(x = county_borders, y = plotData_age, by = 'subregion')
        setDT(test)
        # browser()
          p <- ggplot(test, aes(long, lat, group = group, fill = `Change in # of Licensed Beds`)) +
          geom_polygon(colour = "black") +
          coord_quickmap() +
            scale_fill_fermenter(
              type = 'div',
              limits = c(-100, 100),
              breaks = seq(-120, 80, 24),
              palette = 'Reds',
              na.value = 'grey50',
              direction = 1
            ) +
            theme(
              legend.position = "none",
              legend.title = element_blank(),
              legend.key.width = unit(2,'cm'),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank()
            )
          if (top_plot) {
            p <- p + ggtitle(label = paste(age, 'Patients'))
          }
          return(p)
        }
      },
      .envir = plotList_env)
    if(total_displaced){
      return(plotData[change > 0,sum(change)])
    }else if(counts_by_age){
      ret <- data.table(t(rbindlist(plotList)))
      names(ret) <- unlist(ret[1,])
      ret <- ret[2,]
      return(ret)
    }else if (generate_plot) {
      for (i in seq_along(plotList)){
        assign(x = paste0('p',i),value = plotList[[i]])
      }
      if(top_plot){
        return(gridExtra::grid.arrange(p1 , p2 , p3 , p4,nrow = 1,ncol = 4))
      }else{
        return(lemon::grid_arrange_shared_legend(p1 , p2 , p3 , p4,nrow = 1,ncol = 4,position = 'bottom'))
      }
    }
  }