plotBedShift <-
  function(sol,
           df = siteInfo,
           hosplist = readRDS(file.path('.','Data','plotting_utilities','hosplist.rds')),
           facility_locations = readRDS(file.path(".", "Data", "plotting_utilities", "coordinates.rds")),
           aliases = readRDS(file.path('.','Data','plotting_utilities','Hospital Aliases.rds'))){

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
    plotList <- lapply(
      X = c('Adult', 'Adolescent', 'Pediatric', 'Geriatric'),
      FUN = function(age) {
        plotData_age <-
          plotData[grepl(pattern = age, x = Bed_Group),
                   ][, `:=`(subregion = tolower(gsub(pattern = ' County',
                                                     replacement = '',
                                                     x = county)))
                     ][, .(`Change in # of Licensed Beds` = sum(change)), by = subregion]
        
        county_borders <- map_data('county', 'minnesota')
        test <-
          left_join(x = county_borders, y = plotData_age, by = 'subregion')
        
        return(
          ggplot(test, aes(long, lat, group = group, fill = `Change in # of Licensed Beds`)) +
          geom_polygon(colour = "black") +
          coord_quickmap() +
          scale_fill_gradient2(
            limits = as.numeric(plotData[,lapply(.SD,range),.SDcols = 'change'][,change]),
            mid = 'grey',
            low = "darkred",
            high = "darkblue",
            guide = "colorbar",
            na.value = "white") +
            ggtitle(label = paste('Licensed for',age, 'Patients')) +
            theme(legend.position = "none",
                  axis.title = element_blank())
        )
      })
    for (i in seq_along(plotList)){
      assign(x = paste0('p',i),value = plotList[[i]])
    }
    browser()
    return(lemon::grid_arrange_shared_legend(p1 + p2 + p3 + p4,ncol = 2,position = 'bottom'))
}