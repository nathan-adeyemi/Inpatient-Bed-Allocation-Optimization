# Set the value of temp_folder ----------------------------
if(exists('temp_folder',envir = .GlobalEnv)){
  temp_folder <- get('temp_folder',envir = .GlobalEnv)
} else {
  temp_folder <-  temp_folder <- file.path(".",'Simulation and Alternatives','Validation Results',
                                           paste('temp_results',format(Sys.Date(),"%m_%d"),sep = '_'))
}

# Remove any user-defined functions in the environment ------------------
if (exists('full_sim')){
  if(exists('test_results')){
    fun_list <- setdiff(lsf.str(),'test_results')
  }else{
    fun_list <- lsf.str()
  }
  rm(list = fun_list)
}

# Simulation Call and Output Processing Functions --------------------------

sim_code_path <- file.path(".","Minnesota MH Network Simulation.R")

# Custom Functions for analyzing the Mental Health Simulation
`%+%` <- function(x,y){
  eval.parent(substitute(x <- x + y))
}
`%*%` <- function(x, y) {
  eval.parent(substitute(x <- x * y))
}
`%c%` <- # helper function to find list elements with the same name
  function(x, n) {
    sapply(x, `[[`, n)
  }
remove_zeros <- function(data) {
  return(data[data > 0])
}

today <- Sys.Date()

reap_zombies <-
  function() {
    # Kill left over R processes after parallel computing
    
    if (Sys.info()['sysname'] == 'Darwin') {
      ids <-
        ps() %>% setDT() %>% {
          function(input)
            input[name == 'rsession', pid] %>% as.integer()
        }()
    } else if (Sys.info()['sysname'] == 'Windows') {
      ids <-
        ps() %>% setDT() %>% {
          function(input)
            input[name == 'Rscript.exe', pid] %>% as.integer()
        }()
      
      if (length(ids) > 1) {
        id_list <- ps()[['pid']] %>% unlist()
        for (i in seq_along(ids)) {
          tryCatch({
            ps_kill(ps_handle(ids[i]))
          }, error = function(e) {
            cat("ERROR :", conditionMessage(e), "\n")
          })
        }
      }
    }
  }
#Remove outliers from mayo results
remove_outliers <- function(x, cutoff = 0.025) {
  ub <- quantile(x, 1 - cutoff, na.rm = T)
  lb <- quantile(x, cutoff, na.rm = T)
  return(x[x > lb & x < ub])
}

validate_fun <- function(text,
                         true_val,
                         data = NA,
                         alpha = 0.05,
                         differences = F) {
  if (!is.na(data)) {
    # T.test for if the simulation distribution contains the true parameter
    if (length(data) < 10) {
      data <- rep(data, 100000000)
    }
    return(tryCatch({
      t.test(data, mu = true_val)$p.value > alpha
    },
    error = function(e) {
      err <- cat("ERROR :", conditionMessage(e))
      return (err)
    }))
  } else{
    if (is.character(text)) {
      values <-
        str_split(text, ",|\\(|\\)| ")[[1]] %>%
        as.numeric() %>%
        na.omit()
    } else if (is.list(text) | is.numeric(text)) {
      values <- unlist(text)
    }
    
    min_val <- min(values)
    max_val <- max(values)
    
    if (differences == T) {
      if((true_val > max_val)){
        return(true_val - max_val)
      } else if (true_val < min_val) {
        return(true_val - min_val)
      } else {
        return(0)
      }
    } else {
      return(true_val <= max_val &
               true_val >= min_val)
    }
  }
}

swfun <- Vectorize(function(i){
  if(grepl('Mayo',i)){
    return(switch(i,
                  "Mayo Rochester Pediatric/Adolescent" = "Child/Adolescent Psych",
                  "Mayo Rochester Adult" = "Adult Psych",
                  "Mayo Rochester Geriatric" = "Geriatric/Med Psych"))
  } else {
    return(switch(i,
                  "Adolescent" = "Child/Adolescent Psych",
                  "Child" = "Child/Adolescent Psych",
                  "Adult" = "Adult Psych",
                  "Geriatric" = "Geriatric/Med Psych"))
  }
})

sim_val_subfunction <-
  function(df,
           val_env_df,
           metric,
           val_group = NA,
           val_function = {
             function(x)
               mean(x, na.omit = T)
           },
           use_type = TRUE,
           resource_val = F) {
    if (!any(is.na(val_group))) {
      df[, `Vulnerable Patient` := fifelse(Age %in% val_group, T, F)]
      if (metric == 'count')
        df <- df[, .(count = sum(count,na.rm = T)),
                 by = list(replication, day_num, `Vulnerable Patient`)]
    } else if (resource_val){
      df[,`Vulnerable Patient` :=  resource]
    } else {
      df[, `Vulnerable Patient` := Age]
    }
    if (use_type) {
      ret <-
        rbind(df[, .(`Simulation Confidence Interval` = 
                       sapply(
                         .SD,
                         FUN = function(i)
                           one.boot(data = i,
                                    FUN = val_function,
                                    R = 500)$t0
                       )), 
                 .SDcols = metric, by = list(`Vulnerable Patient`, replication, type)
                 ],df[, .(`Simulation Confidence Interval` = 
                            sapply(
                              .SD,
                              FUN = function(i)
                                one.boot(data = i,
                                         FUN = val_function,
                                         R = 500)$t0
                            )), 
                      .SDcols = metric, by = list(replication, type)
                      ],df[, .(`Simulation Confidence Interval` = 
                                 sapply(
                                   .SD,
                                   FUN = function(i)
                                     one.boot(data = i,
                                              FUN = val_function,
                                              R = 500)$t0
                                 )), 
                           .SDcols = metric, by = list(`Vulnerable Patient`, replication)
                           ],fill = T)[val_env_df, `Target Value` := Target, on = c(`Vulnerable Patient` = 'val_group', type = 'type')
                                       ][!is.na(`Target Value`),][, .(`Simulation Confidence Interval` = ci_as_text(signif(t.test(`Simulation Confidence Interval`, conf.level  = .95)$conf.int, 4)),
                                                                      `Target Value` = unique(`Target Value`),
                                                                      `Statistically Valid?` = t.test(x = `Simulation Confidence Interval`,mu = unique(`Target Value`))$p.value > 0.05,
                                                                      `CI contains True Value?` = {Vectorize(validate_fun)}(text = list(signif(t.test(`Simulation Confidence Interval`, conf.level  = .95)$conf.int, 4)), 
                                                                                                                            true_val = unique(`Target Value`))),
                                                                  by = list(`Vulnerable Patient`, type)
                                                                  ][CJ(`Vulnerable Patient` = `Vulnerable Patient`,type = type,unique = TRUE), on = .(`Vulnerable Patient`, type)
                                                                    ][!is.na(`Target Value`),][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                                                                                                    true_val = `Target Value`,
                                                                                                                                    differences = T)]
    } else {
      ret <- rbind(df[, .(`Simulation Confidence Interval` = 
                            sapply(.SD, function(x)
                              one.boot(data = na.omit(x),
                                       FUN = val_function,
                                       R = 500)$t0)), 
                      .SDcols = metric, by = list(`Vulnerable Patient`, replication)],
                   df[,.(count = sum(count)), by = list(replication,day_num)
                      ][, .(`Simulation Confidence Interval` = 
                              sapply(.SD, function(x)
                                one.boot(data = na.omit(x),
                                         FUN = val_function,
                                         R = 500)$t0)), 
                        .SDcols = metric, by = list(replication)
                        ],fill = T)[val_env_df, `Target Value` := Target, on = c(`Vulnerable Patient` = 'val_group')
                                    ][,.(`Simulation Confidence Interval` = ci_as_text(signif(t.test(`Simulation Confidence Interval`, conf.level  = .95)$conf.int, 4)),
                                         `Target Value` = unique(`Target Value`),
                                         `Statistically Valid?` = t.test(x = `Simulation Confidence Interval`,
                                                                         mu = unique(`Target Value`))$p.value > 0.05,
                                         `CI contains True Value?` = {Vectorize(validate_fun)}(text = list(signif(t.test(`Simulation Confidence Interval`, conf.level  = .95)$conf.int, 4)),
                                                                                               true_val = unique(`Target Value`))),
                                      by = list(`Vulnerable Patient`),
                                      ][CJ(`Vulnerable Patient` = `Vulnerable Patient`, unique = TRUE), 
                                        on = .(`Vulnerable Patient`)
                                        ][!is.na(`Target Value`),
                                          ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                                                 true_val = `Target Value`,
                                                                                 differences = T)]
    }
    return(ret) 
  }


validate_results <- function(patients_df,resource_df,just_mayo = T){
  
  list2env(readRDS(
    file = file.path(
      ".",
      "Data",
      "Function Requirements",
      "MH_Network_sim_input_list.rds"
    )
  ), envir = environment())
  
  val_env <- readRDS(file.path(".","Data Analysis","Validation Metric Dataframes.rds"))
  
  if(just_mayo){
    data <-
      patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)
                  ][, day_num := sim_date(3600 * Enter.Timestamp)
                    ][,`:=`(
                      Age = as.factor(Age),
                      replication = as.factor(replication),
                      type = as.factor(type)
                    )]
    
    mayo_ip <- patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester",Transfer.Site)
                           ][, day_num := sim_date(3600 * Enter.Timestamp)
                             ][,`:=`(
                               Age = as.factor(Age),
                               replication = as.factor(replication),
                               type = as.factor(type),
                               unit = {
                                 Vectorize(function(i)
                                   switch(
                                     as.character(i),
                                     'Adult' = 'Mayo Rochester Adult',
                                     'Adolescent' = 'Mayo Rochester Pediatric/Adolescent',
                                     'Child' = 'Mayo Rochester Pediatric/Adolescent',
                                     'Geriatric' = 'Mayo Rochester Geriatric'
                                   ))}(Age))]
    
    resource_df <- resource_df[grepl('Mayo Rochester',resource,ignore.case = T),]
  }else{
    data <-
      patients_df[,day_num := sim_date(3600 * Enter.Timestamp)
                  ][Age = as.factor(Age),
                    replication = as.factor(replication),
                    type = as.factor(type)]
  }
  
  validation_frames <- list() #Collection of all validation data.frames
  
  # Percentage of ED patients_df transferred somewhere else
  validation_frames$`Mayo Transfers Out by Day` <- 
    sim_val_subfunction(
      df = data[type == 'Transfer',.(count = .N), by = list(replication,Age,day_num)
                ][CJ(day_num = seq(max(day_num,na.rm = TRUE)),
                     replication = replication, Age = Age, unique = TRUE),
                  on = .(Age,replication,day_num)
                  ][!(is.na(day_num)),
                    ][is.na(count),count := 0][order(day_num)],
      val_env_df = val_env$transfer_out_rate,
      metric = 'count',
      val_group = c('Adolescent', 'Child', 'Geriatric'),
      val_function = {
        function(x)
          mean(x, na.rm = T)
      },
      use_type = FALSE)[, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                             true_val = `Target Value`,
                                                             differences = T)
                        ][,`% Error` := Delta/`Target Value` * 100]
  
  # Rates of external transfers into Mayo Clinic inpatient beds
  validation_frames$`Mayo Transfers in by Day` <-
    sim_val_subfunction(
      df = copy(mayo_ip[!grepl(pattern = "Mayo Clinic Hospital - Rochester", Site.Entered)]
      )[order(IP.Arrival.Timestamp), day_num := sim_date(IP.Arrival.Timestamp * 3600), by = list(replication)
        ][, .(count = .N), by = list(Age, replication, day_num)
          ][CJ(day_num = seq(min(day_num, na.rm = T),max(day_num, na.rm = T)),
               replication = replication,
               Age = Age,
               unique = TRUE),on = .(Age, replication, day_num)
            ][is.na(count), count := 0],
      val_env_df = val_env$Transfer_to_Mayo,
      metric = 'count',
      val_group = c('Adolescent', 'Child', 'Geriatric'),
      val_function = {
        function(x)
          mean(x, na.rm = T)
      },
      use_type = FALSE
    )[, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                           true_val = `Target Value`,
                                           differences = T)
      ][,`% Error` := Delta/`Target Value` * 100]
  
  origin_date <- as.POSIXct("2018-12-01 00:00:00", tz = "UTC") - (3600 * 24 * warm_period)
  resource_df <-
    copy(resource_df)[order(replication,time, system)
                      ][,`:=`(date_time = as.datetime(actual_ip_start * 3600,origin_date),
                              prog_day = as.numeric(as.factor(lubridate::date(as.datetime(actual_ip_start * 3600,origin_date))))
                      )
                      ][, `:=`(prev = data.table::shift(system, 1),
                               occupancy = 100 * (server/capacity),
                               Delta = abs(as.numeric(difftime(date_time, data.table::shift(date_time, 1), 
                                                               units = 'hours')))), 
                        by = list(resource, replication)
                        ][, change := system - prev,
                          by = list(resource, replication)
                          ][, prev := NULL
                            ][,`:=`(`ED Patient` = T, `External Transfer` = F)
                              ][grepl('IP_Unit',patient,ignore.case = T),`ED Patient` := F
                                ][!grepl('Mayo Clinic Hospital - Rochester',patient,ignore.case = T) & 
                                    `ED Patient` == T,`External Transfer` := T]
  
  validation_frames$`IP Unit Queueing Metrics` <-
    rbindlist(
      list(
        #Inpatient Unit Occupancy Rate
        copy(resource_df)[!is.na(Delta),
                          .(Target = signif(weighted.mean(occupancy, Delta), digits = 4)),
                          by = list(resource, replication)
                          ][, .(`Simulation Confidence Interval` = t.test(Target, conf.level = .95)$conf.int %>% 
                                  signif(digits = 4) %>% 
                                  {function(x) paste0("(", x[1], ",", x[2], ")")}()), by = resource
                            ][, resource := {Vectorize(swfun)}(resource)
                              ][, Measure := 'Occupancy Rate'],
        
        #Inpatient Unit Arrival Rate
        copy(resource_df)[change > 0,
                          ][, .(Count = sum(change)), by = list(prog_day, resource, replication)
                            ][CJ(
                              prog_day = prog_day,
                              resource = resource,
                              replication = replication,
                              unique = TRUE),on = .(prog_day, resource, replication)
                              ][is.na(Count), Count := 0
                                ][,.(Count = one.boot(Count,mean,500)$t0),by = list(replication,resource)
                                  ][, .(`Simulation Confidence Interval` = t.test(Count, conf.level = .95)$conf.int %>%
                                          signif(digits = 4) %>%
                                          {function(x) paste0("(", x[1], ",", x[2], ")")}()), by = resource
                                    ][, resource := {Vectorize(swfun)}(resource)
                                      ][, Measure := 'Arrival Rate'],
        # Inpatient Unit Length of Stay
        copy(resource_df)[!is.na(actual_ip_start),time := actual_ip_start
                          ][, .(ip_LoS = abs( abs(max(time) - min(time))),
                                resource = resource), by = list(replication,patient)
                            ][, .(ip_LoS = one.boot(ip_LoS, mean, 500, na.rm = T)$t0), by = list(resource,replication)
                              ][,.(`Simulation Confidence Interval` = t.test(ip_LoS,conf.level = .95)$conf.int %>% 
                                     signif(digits = 4) %>% 
                                     {function(x) paste0("(", x[1], ",", x[2], ")")}()),by = resource
                                ][, Measure := "Length of Stay"
                                  ][,resource := {Vectorize(swfun)}(resource)]),
      use.names = T
    )[val_env$ip_unit_metrics, Target := Target, on = c(Measure = 'variable', resource = 'location_description')
      ][,`CI contains True Value?` := {Vectorize(validate_fun)}(`Simulation Confidence Interval`,Target)
        ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                               true_val = Target,
                                               differences = T)]
  
  validation_frames$`Arrival Rates by Patient Type` <- #Inpatient Unit Arrival Rate
    copy(resource_df
    )[change > 0,
      ][, .(Count = .N), by = list(prog_day, resource, replication, `External Transfer`, `ED Patient`)
        ][CJ(
          prog_day = prog_day,
          resource = resource,
          replication = replication,
          `External Transfer` = `External Transfer`,
          `ED Patient` = `ED Patient`,
          unique = TRUE
        ),
        on = .(prog_day,
               resource,
               replication,
               `ED Patient`,
               `External Transfer`)
        ][is.na(Count), Count := 0
          ][, .(Count = one.boot(Count, mean, 500)$t0),
            by = list(replication,
                      resource,
                      `External Transfer`, 
                      `ED Patient`)
            ][, .(`Simulation Confidence Interval` = t.test(Count, conf.level = .95)$conf.int %>% 
                    signif(digits = 4) %>%
                    {function(x) paste0("(", x[1], ",", x[2], ")")}()),
              by = list(resource, `External Transfer`, `ED Patient`)
              ][, resource := {Vectorize(swfun)}(resource)
                ][, variable := 'Arrival Rate'
                  ][val_env$split_arrival_rates, `Target Value` := as.numeric(Target), 
                    on = c(resource = 'val_group',
                           `External Transfer` = 'transfer', 
                           `ED Patient` = 'ed_patient')
                    ][!is.na(`Target Value`),
                      ][, `CI contains True Value?` := {Vectorize(validate_fun)}(`Simulation Confidence Interval`, `Target Value`)
                        ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                               true_val = `Target Value`,
                                                               differences = T)
                          ][,`% Error` := Delta/`Target Value` * 100
                            ][,setnames(.SD,'resource','IP Unit')]
  
  # Compare mean simulated total coordination time vs Mayo total ED wait (by placement type)
  validation_frames$`Average ED Patient Wait Times` <-
    data.table(
      pivot_wider(
        # rbindlist(
        #   list(
        #     sim_val_subfunction(
        #       df = data,
        #       val_env_df = val_env$ed_wait_mean,
        #       metric = 'total_wait_time',
        #       val_group = c('Adolescent', 'Child', 'Geriatric'),
        #       val_function = {
        #         function(x)
        #           mean(x, na.rm = T)
        #       }
        #     )[, val_metric := 'Disposition -> Departure'],
        sim_val_subfunction(
          df = data,
          val_env_df = val_env$coord_mean,
          metric = 'total_wait_time',
          val_group =
            c('Adolescent', 'Child', 'Geriatric'),
          val_function = {
            function(x)
              mean(x, na.rm = T)
          }
        )[, val_metric := 'Disposition -> Bed Assignment'],
        #   ),
        #   use.names = T
        # ),
        id_cols = c(`Vulnerable Patient`, type, `Simulation Confidence Interval`),
        names_from = val_metric,
        values_from = c(`Target Value`, `CI contains True Value?`),
        names_glue = "{val_metric} {.value}"
      )
    ) %>% {function(df) df[, c(setdiff(colnames(df), grep('disp', colnames(df), value = T)),
                               sort(grep('disp', colnames(df), value = T))), with = FALSE]}()
  
  validation_frames$`Median ED Patient Wait Time`<-
    data.table(
      pivot_wider(
        # rbindlist(
        #   list(
        #     sim_val_subfunction(
        #       df = data,
        #       val_env_df = val_env$ed_wait_median,
        #       metric = 'total_wait_time',
        #       val_group =
        #         c('Adolescent', 'Child', 'Geriatric'),
        #       val_function = {
        #         function(x)
        #           median(x, na.rm = T)
        #       }
        #     )[, val_metric := 'Disposition -> Departure'],
        sim_val_subfunction(
          df = data,
          val_env_df = val_env$coord_median,
          metric = 'total_wait_time',
          val_group =
            c('Adolescent', 'Child', 'Geriatric'),
          val_function = {
            function(x)
              median(x, na.rm = T)
          }
        )[, val_metric := 'Disposition -> Bed Assignment'],
        #   ),
        #   use.names = T
        # ),
        id_cols = c(`Vulnerable Patient`, type, `Simulation Confidence Interval`),
        names_from = val_metric,
        values_from = c(`Target Value`, `CI contains True Value?`),
        names_glue = "{val_metric} {.value}"
      )
    ) %>% {function(df) df[, c(setdiff(colnames(df), grep('disp', colnames(df), value = T)),
                               sort(grep('disp', colnames(df), value = T))), with = FALSE]}()
  validation_frames$`Average ED Patient Wait Times` <-
    validation_frames$`Average ED Patient Wait Times`[, `:=`(
      `Delta(Disp -> Assign)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                          true_val = `Disposition -> Bed Assignment Target Value`,
                                                          differences = T)
      # `Delta(Disp -> Departure)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
      #                                               true_val = `Disposition -> Departure Target Value`,
      #                                               differences = T)
    )
    ][, `:=`(`% Error(Disp -> Assignment)` = `Delta(Disp -> Assign)` /  `Disposition -> Bed Assignment Target Value` * 100
             # `% Error(Disp -> Departure)` = `Delta(Disp -> Departure)` / `Disposition -> Departure Target Value` * 100
    )]
  
  validation_frames$`Median ED Patient Wait Time` <-
    validation_frames$`Median ED Patient Wait Time`[, `:=`(
      `Delta(Disp -> Assign)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                                          true_val =  `Disposition -> Bed Assignment Target Value`,
                                                          differences = T)
      # `Delta(Disp -> Departure)` = {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
      #                                               true_val = `Disposition -> Departure Target Value`,
      #                                               differences = T)
    )
    ][, `:=`(`% Error(Disp -> Assignment)` = `Delta(Disp -> Assign)` / `Disposition -> Bed Assignment Target Value` * 100
             # `% Error(Disp -> Departure)` = `Delta(Disp -> Departure)` / `Disposition -> Departure Target Value` * 100
    )]
  
  # Average adult number of transfer request rejections
  validation_frames$n_rejects <-
    sim_val_subfunction(
      df = copy(data)[type == 'Transfer', ],
      val_env_df = val_env$rejects,
      metric = 'Times.Rejected',
      val_group = c('Adolescent', 'Child', 'Geriatric'),
      val_function = {
        function(x)
          mean(x, na.rm = T)
      },
      use_type = TRUE
    )[type == 'Transfer'
      ][, Delta := {Vectorize(validate_fun)}(text = `Simulation Confidence Interval`,
                                             true_val = `Target Value`,
                                             differences = T)
        ][,`% Error` := Delta/`Target Value` * 100]
  
  # validation_frames$non_placed_rate <- patients_df[,.(no_placement = sum(as.numeric(is.na(coord_time))),Count = .N), by = list(Age,replication)
  #                                               ][,Rate := 100 * no_placement/Count
  #                                                 ][!is.na(replication),setorderv(.SD,c('Age','replication'))
  #                                                   ][,.(Non_Placement_Rate = list(signif(boot_confInt_val(Rate,conf.level = 0.95)$conf.int,3))) ,by = list(Age)]
  
  # patients_df[grepl(pattern = "Mayo Clinic Hospital - Rochester", Transfer.Site),
  #             ][order(Enter.Timestamp),
  #               ][, `:=`(
  #                  resource = {Vectorize(swfun)}(Age),
  #                  LoS = abs(Finish.Timestamp - IP.Arrival.Timestamp)
  #                  )][, .(Avg = one.boot(na.omit(LoS), mean, 1000)$t0),by = list(replication, resource)
  #                     ][, .(`Simulation Confidence Interval` = t.test(Avg, conf.level = .95)$conf.int %>% signif(digits = 4) %>% 
  #                              {function(x) paste0("(", x[1], ",", x[2], ")")}()),
  #                       by = resource]
  
  return(validation_frames)
  # Code to use up to the 99th percentile of coordination time values: coord_time[coord_time < quantile(coord_time,.99,na.rm = T)]
}


progressive_mean <- function(df){ 
  sapply(
    X = seq(nrow((df))),
    FUN = function(i)
      mean(df[seq(i), 'wait_times'] %>% unlist(), na.rm = T)
  )
}

rep_prog_func <-
  function(x) {
    data.table(Ts = as.double(x$IP.Arrival.Timestamp),
               Average.TTP = progressive_mean(x))
  }

determine_params <- function(patients){
  
  # Number of Replications Analysis Plot
  ages = c('Adolescent','Adult','Child','Geriatric')
  numIters <- max(patients$replication)
  
  # Plots average wait time against simulation time to find warmup
  listpatients <- patients %>% split(.,patients$replication)
  
  if(Sys.info()['sysname'] != 'Darwin'){
    cl = makeCluster(detectCores(logical = T)-1,outfile = "")
    registerDoParallel(cl)
    repProgMeans <- foreach(i = listpatients,
                            .export = c('listpatients','progressive_mean'),
                            .packages = c('data.table','dplyr')) %dopar% rep_prog_func(i)
    stopImplicitCluster()
  } else {
    repProgMeans <- mclapply(listpatients,FUN = function(i) rep_prog_func(i),mc.cores = detectCores() - 2)
  }
  
  # Plots output statistic mean with respect to time
  repProgMeansDf <- rbindlist(repProgMeans,idcol = 'Rep')
  ggplot(repProgMeansDf,aes(x = Ts, y = Average.TTP)) +
    geom_line(color = 'blue', alpha = .1, aes(group = Rep)) + geom_smooth(se = TRUE, color = 'black',size = 0.6)
  
  # Plots of average wait time by number of replications
  plot_data <- sapply(ages, function(age) sapply(seq(max(numIters)),
                                                 function(i) mean(replicate(10, mean(patients[replication %in% sample(unique(patients$replication),size = i) &
                                                                                                Age ==age, wait_times],na.rm = T))))) %>%
    data.table() %>% mutate(Replication = seq(numIters)) %>% pivot_longer(!Replication)
  
  overall_plot <- sapply(seq(max(numIters)),function(i) mean(replicate(10, mean(patients[replication %in% sample(unique(patients$replication),size = i), wait_times],na.rm = T)))) %>%
    data.table() %>% mutate(Replication = seq(numIters)) %>% pivot_longer(!Replication)
  
  ggplot(plot_data,aes(x = Replication, y = value, color = name)) + geom_line()
  ggplot(overall_plot, aes(x = Replication, y = value)) + geom_line()
}


# Results Functions -------------------------------------------------------
extract_results <- function(df,
                            metric,
                            result_group = NA,
                            result_function = {
                              function(x)
                                mean(x, na.rm = T)
                            },
                            use_type = TRUE,
                            separate_vulnerable = T,
                            resource = F) {
  
  if (separate_vulnerable) {
    df[, `Vulnerable Patient` := Age != 'Adult',by = list(Sim.ID,replication)]
  } else if (resource) {
    df[, `Vulnerable Patient` :=  resource]
  } else {
    df[, `Vulnerable Patient` := Age]
  }
  if (metric == 'count'){
    df <- df[, .(count = sum(count,na.rm = T)),
             by = list(replication, day_num, `Vulnerable Patient`)]
  } 
  if (resource){
    df <- df[, `:=`(
      treat_adults = grepl(pattern = 'Adult', resource, ignore.case = T),
      treat_children = grepl(pattern = 'Children|Pediatric', resource, ignore.case = T),
      treat_adolescent = grepl(pattern = 'Adolescent', resource, ignore.case = T),
      treat_geriatric = grepl(pattern = 'Geriatric', resource, ignore.case = T)
    )]
    ret = rbind(
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_adults,replication)
         ][treat_adults == T,
           ][,`:=`(Age = 'Adult',treat_adults = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_adolescent,replication)
         ][treat_adolescent == T,
           ][,`:=`(Age = 'Adolescent',treat_adolescent = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_children,replication)
         ][treat_children == T,
           ][,`:=`(Age = 'Child',treat_children = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(treat_geriatric,replication)
         ][treat_geriatric == T,
           ][,`:=`(Age = 'Geriatric',treat_geriatric = NULL)],
      df[, .(`Simulation Confidence Interval` =
               sapply(
                 X = .SD,
                 FUN = function(i)
                   one.boot(data = i,
                            FUN = result_function,
                            R = 500)$t0
               )),
         .SDcols = metric,
         by = list(replication)
         ][,`:=`(Age = 'All')],
      fill = T)
    ret <- ret[, .(Mean = one.boot(data = `Simulation Confidence Interval`,FUN = mean,R = 500,na.rm = T)[['t0']],`Simulation Confidence Interval` = 
                     ci_as_text(signif(x = t.test(`Simulation Confidence Interval`,conf.level  = .95)$conf.int,
                                       digits = 4))),by = Age]
  }else if (use_type) {
    ret <-
      rbind(df[, .(`Simulation Confidence Interval` = 
                     sapply(
                       .SD,
                       FUN = function(i)
                         one.boot(data = na.omit(i),
                                  FUN = result_function,
                                  R = 500)$t0
                     )), 
               .SDcols = metric, by = list(`Vulnerable Patient`, replication, type)
               ],
            df[, .(`Simulation Confidence Interval` =
                     sapply(
                       .SD,
                       FUN = function(i)
                         one.boot(data = na.omit(i),
                                  FUN = result_function,
                                  R = 500)$t0
                     )),
               .SDcols = metric, by = list(replication, type)], 
            df[, .(`Simulation Confidence Interval` =
                     sapply(
                       .SD,
                       FUN = function(i)
                         one.boot(data = na.omit(i),
                                  FUN = result_function,
                                  R = 500)$t0
                     )),  
                         .SDcols = metric, by = list(`Vulnerable Patient`, replication)
                         ],fill = T)[, .(Mean = one.boot(data = `Simulation Confidence Interval`,FUN = mean,R = 500,na.rm = T)[['t0']],
                                         `Simulation Confidence Interval` = ci_as_text(signif(t.test(`Simulation Confidence Interval`, 
                                                                                                     conf.level  = .95)$conf.int, 4))),
                                     by = list(`Vulnerable Patient`, type)
                                     ][CJ(`Vulnerable Patient` = `Vulnerable Patient`,
                                          type = type,unique = TRUE), 
                                       on = .(`Vulnerable Patient`, type)]
  } else {
    ret <- rbind(df[, .(`Simulation Confidence Interval` = 
                          sapply(.SD, function(x)
                            one.boot(data = na.omit(x),
                                     FUN = result_function,
                                     R = 500)$t0)), 
                    .SDcols = metric, by = list(`Vulnerable Patient`, replication)],
                 df[, .(`Simulation Confidence Interval` = 
                          sapply(.SD, function(x)
                            one.boot(data = na.omit(x),
                                     FUN = result_function,
                                     R = 500)$t0)), 
                    .SDcols = metric, by = list(replication)
                    ],fill = T
    )[, .(Mean = one.boot(data = `Simulation Confidence Interval`,FUN = mean,R = 500,na.rm = T)[['t0']],
          `Simulation Confidence Interval` = ci_as_text(interval = signif(x = t.test(x = `Simulation Confidence Interval`,
                                                                                     conf.level  = .95)$conf.int, 4))),
      by = list(`Vulnerable Patient`)]
  }
  return(ret) 
}

# Results Visualization Functions ------------------------------------------
results_boxplot <- function(data,col){ # results_boxplot: creates boxplots for results
  ggplot(data,aes(x = log2(wait_times),y = col, group = col)) +
    geom_boxplot() +
    rotate() +
    scale_x_continuous(limits = c(-3.75,7))
}

results_density <- function(data){ # results_densityt: creates desnity plot for results
  lapply(seq(8),function(i) ggplot(data[[i]],aes(x = wait_times)) +
           geom_density() +
           scale_x_continuous(limits = c(0,100)) +
           ggtitle(paste0('Wait Time Distribution (Send ',i,' Requests Concurrently)'))) %>%
    grid.arrange(grobs = .,nrow = 4)
}

plot_usage <- function(resources,patients){
  resource_by_fac <- split(resources,f = resources$resource)
  resource_by_fac <- lapply(resource_by_fac,function(df){
    class(df) = c('resources','data.frame')
    return(df)
  })
  
  fac_usage_plots <- lapply(resource_by_fac,FUN = function(data) plot(data,metric = 'usage'))
  ggsave( # Makes a pdf of all resource usage plots
    filename = file.path(".",'Simulation and Alternatives','Validation Results') %>%
      file.path(.,list.files(.)[length(list.files(.))]) %>% file.path(.,list.files(.)[length(list.files(.))],"IP Unit Utilization Plots.pdf"),
    plot = marrangeGrob(fac_usage_plots, nrow=1, ncol=1),
    width = 15, height = 9)
}

#Validation Target Value Calculation Functions-----------
samplewmean <- Vectorize(function(d, i, j) {
  d <- d[i]
  w <- j[i]
  return(weighted.mean(d, w))
})

weighted.var = function(x,w,type="reliability") {
  m=weighted.mean(x,w)
  if(type=="frequency"){ return( sum(w*(x-m)^2)/(sum(w)-1) ) }
  else { return( sum(w*(x-m)^2)*sum(w)/(sum(w)^2-sum(w^2)) ) }
}

weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
  nx <- length(x)
  df <- nx - 1
  vx <- weighted.var(x = x, w = weights) 
  mx <- weighted.mean(x, weights)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint * stderr
}

extract_time_validation_metric <-
  function(data,
           val_group = NA,
           metric = 'disp_to_dep',
           val_func = {
             function(i)
               mean(i, na.rm = T)
           }) {
    bs <- function(x) {
      FUNCTION = val_func
      interval = 95
      interval_type = 'basic'
      # boot_confInt_target_val
      boot_name <- switch(
        interval_type,
        'basic' = 'basic',
        'perc' = 'percent',
        'norm' = 'normal',
        'stud' = 'student',
        'bca' = 'bca'
      )
      endpoint_indices <-
        ifelse(rep(interval_type == 'norm', 2), c(2, 3), c(4, 5))
      
      if (length(unique(x)) >= 10) {
        x = remove_outliers(x)
      }
      
      p <- tryCatch({
        signif(boot.ci(
          one.boot(x, FUNCTION, 500),
          conf = interval * 0.01,
          type = interval_type
        )[[boot_name]][endpoint_indices],
        4) %>%
          {function(x) paste0('(', x[1], ",", x[2], ")")}()
      },
      error = function(e) {
        err <- cat("ERROR :", conditionMessage(e))
        return (err)
      })
      
      return(p)
    }
    
    if (!any(is.na(val_group))) {
      data$val_group <-
        sapply(data[, age_group], function(i)
          any(i == val_group)) %>% data.table()
      if(metric == 'count'){
        data <- rbind(data[,.(count = sum(count)),by = list(val_group,prog_day_I)],data[,.(count = sum(count)),by = list(prog_day_I)],fill = T)
      }
    } else {
      data[, val_group := age_group]
    }
    
    # Type refers to whether patients are Internally placed or Transferred (not all validation data frames have this)
    if (!('type' %in% colnames(data))) {
      data[, type := 'All']
    }
    x <-
      rbindlist(list(data[, .(
        Target = sapply(.SD, function(x) {
          if (!any(c('count','rejections') == metric)) {
            x = remove_outliers(x)
          }
          return(one.boot(x, val_func, 1000)$t0)
        }),
        CI = lapply(.SD, bs),
        Count = .N
      ),
      .SDcols = metric, by = list(val_group, type)],
      data[, .(
        Target =
          sapply(.SD, function(x) {
            if (!any(c('count','rejections') == metric)) {
              x = remove_outliers(x)
            }
            return(one.boot(x, val_func, 1000)$t0)
          }),
        CI = lapply(.SD, bs),
        Count = .N
      ),
      .SDcols = metric, by = list(val_group)],
      data[, .(
        Target =
          sapply(.SD, function(x) {
            if (!any(c('count','rejections') == metric)) {
              x = remove_outliers(x)
            }
            return(one.boot(x, val_func, 1000)$t0)
          }),
        CI = lapply(.SD, bs),
        Count = .N
      ),
      .SDcols = metric, by = list(type)]),
      use.names = T,
      fill = T)[CJ(val_group = val_group,
                   type = type,
                   unique = TRUE),
                on = .(val_group, type)]
    
    if (!('type' %in% colnames(data))) {
      x <- unique(x[, type := NULL])
    }
    return(x[!is.na(Target)])
  }

# Mayo Data reading and formatting functions -----------------------------

# Categorize Ages
age.classify <- function(x) {
  if(is.na(x)){
    y <- 'Adolescent' 
  } else if (x < 12){
    y <- 'Child'
  } else if (x >= 12 & x < 18) {
    y <- 'Adolescent'
  } else if (x >= 18 & x < 65) {
    y <- 'Adult'
  } else{
    y <- 'Geriatric'
  }
  return(y)
}

unit.classify <- function(x) {
  if (grepl('RST ROGE 03 E BEH|Geriatric/Med Psych', x, ignore.case = T)) {
    return('Geriatric/Med Psych')
  } else if (grepl('RST ROGE 01 W BEH|Child/Adolescent Psych', x, ignore.case = T)) {
    return('Child/Adolescent Psych')
  } else if (grepl('RST ROGE 02 E BEH|Adult Psych', x, ignore.case = T)) {
    return('Adult Psych')
  } else if (grepl('RST ROGE 03 W BEH|Mood Disorders Unit', x, ignore.case = T)) {
    return('Mood Disorders Unit')
  } else {
    return('ED')
  }
}


ED.classify <- function(ed) {
  return(switch(
    ed,
    'MCHS NPNH ED' = 'Mayo Clinic Health System in New Praugue',
    'MCHS ALAH ED' = 'Mayo Clinic Health System - Albert Lea and Austin',
    'MCHS LASF ED' = 'Mayo Clinic Health System in LaCrosse (WI)',
    'MCHS RWMC ED' = 'Mayo Clinic Health System in Red Wing',
    'MCHS AUAH ED' = 'Mayo Clinic Health System - Albert Lea and Austin',
    'MCHS MAMH ED' = 'Mayo Clinic Health System in Mankato',
    'MCHS SRSM ED' = 'Mayo Clinic Health System in Springfield',
    'RST ROMB ED' = 'Mayo Clinic Hospital - Rochester',
    'MCHS EULH ED' = 'Mayo Clinic Health System in Eau Claire (WI)',
    'MCHS LCMC ED' = 'Mayo Clinic Health System in Lake City',
    'MCHS OOOH ED' = 'Mayo Clinic Health System in Owatonna',
    'ZZMCHS SFSH ED' = 'Mayo Clinic Health System in Unknown',
    'MCHS BRBH ED' = 'Mayo Clinic Health System in Unknown',
    'MCHS MEMH ED' = 'Mayo Clinic Health System in Menomonie',
    'MCHS FAFH ED' = 'Mayo Clinic Health System in Fairmont',
    'MCHS WCWE ED' = 'Mayo Clinic Health System in Waseca',
    'MCHS SJSJ ED' = 'Mayo Clinic Health System in St. James',
    'MCHS CACF ED' = 'Mayo Clinic Health System in Cannon Falls',
    'MCHS BLBH ED' = 'Mayo Clinic Health System in Bloomer (WI)',
  ))
}

source.classify <- function(origin){
  if(origin == 'Transfer from a Hospital (Different Facility)'){
    return('External Transfer')
  }else if(origin == 'ED'){
    return('ED')
  } else {
    return('Other')
  }
}

replace_health_systems <- function(df) {
  df$facility_contacted[is.na(df$facility_contacted)] <-
    with(df, receiving_facility[is.na(facility_contacted)])
  for (sys in names(health_systems)) {
    systems <- df[grepl(sys, facility_contacted, ignore.case = T), ]
    if (nrow(systems) > 0) {
      for (row in seq(nrow(systems))) {
        temp <- systems[rep(row, length(health_systems[[sys]]))]
        temp$facility_contacted <- health_systems[[sys]]
        df <- rbind(df, temp)
      }
    }
    df <-  df[!grepl(sys, facility_contacted, ignore.case = T)]
  }
  df[, facility_contacted := tolower(facility_contacted)]
  return(df)
}

sub_NonPrivIP <- function(i){
  return(as.numeric(i %in% siteInfo$Facility_name))
}

accept_time_func <- function(x,y){
  if(length(unique(na.omit(x))) != 0){
    return(max(unique(x),na.rm = T))
  }else{
    return(max(y,na.rm = T))
  }
}


find_drive_distance <- Vectorize(function(x, y) {
  if (length(x) == 0 ||
      length(na.omit(y)) == 0 ||
      !(x %in% colnames(distance.matrix)) ||
      !(y %in% colnames(distance.matrix))) {
    return(NA)
  } else{
    return(distance.matrix[x, na.omit(y)[1]])
  }
})

extract_bed <- function(i){
  return(tail(unlist(str_split(i,' ')),1))
}

# Simulation Input Calculation Functions --------------------------------------------------------
calc_coordination_time <- function(df){
  no_availability <- df[grepl('no availability|no beds|no capacity|no (.*) availability|no (.*) available|unavailable|no (.*) capacity|no (.*) beds|no (.*) bed|at capacity',
                              add_info,ignore.case = T) | grepl('\\<full\\>',add_info,ignore.case = T),]
  
  df<- setDT(setdiff(df,no_availability))
  fac_p1 <- df$facility_contacted %>% tolower()
  facs <- table(na.omit(fac_p1))
  names <- names(facs)
  facs <- as.numeric(facs)
  names(facs) <- names
  resList = list()
  
  for(i in seq_along(facs)){
    
    test_name <- names(facs)[i]
    
    if (facs[i] > 1){
      inds <-which(sapply(tolower(df$facility_contacted),function(text) test_name == text)) %>% unname() %>% {function(i) c(i[1],i[length(i)])}()
      temp_time = as.numeric(abs(difftime(time1 = df[inds[2],event_ts],
                                          time2 = df[inds[1],event_ts],
                                          units = 'hours')))
      temp_time <- ifelse(temp_time == 0,NA,temp_time)
      
      names(temp_time) <- test_name
      resList <- append(resList,temp_time)
    } else {
      
      inds <- which(sapply(tolower(df$facility_contacted),function(text) test_name == text)) %>% unname()
      temp_time <-  as.numeric(abs(difftime(time1 = df[inds,event_ts],
                                            time2 = df[inds[1],if_else(!is.na(ed_disp_ts),ed_disp_ts,ts_1)],
                                            units = 'hours')))
      
      temp_time <- ifelse(temp_time == 0,NA,temp_time)
      
      names(temp_time) <- test_name
      
      resList <- append(resList,temp_time)
    }
  }
  return(resList)
}

time_extract <- function(t1,t2,t3){
  t2 <- min(c(t2,t3)[!is.infinite(as.numeric(c(t2,t3)))])
  if (t1 == t2){
    return(NA_real_)
  }else{
    return(abs(as.numeric(difftime(t1,t2,units = 'hours'))))
  }
}

timeOfDay <- function(input){
  if( hour(input) >= 0 & hour(input) < 6 ){
    result = 'Early Morning'
  } else if (hour(input) >= 6 & hour(input) < 12){
    result = 'Morning'
  } else if (hour(input) >= 12 & hour(input) < 18){
    result = 'Afternoon/Evening'
  } else {
    result = 'Night'
  }
  return(result)
}

weekday_factors <- function(data){
  rbind(data[,.(N = sum(N),day_of_week = unique(day_of_week)),by = Date
             ][,.(AvgAdmits = mean(N)),by = day_of_week],
        data[,.(N = sum(N),day_of_week = unique(day_of_week)),by = Date
             ][,.(AvgAdmits = mean(N))],fill = T
  )[,factor := AvgAdmits/(.SD[is.na(day_of_week),AvgAdmits])
    ][!is.na(day_of_week)
      ][order(day_of_week)]
}

time_of_day_factors <- function(data){
  rbind(data[,.(AvgAdmits = mean(N)), by = time_of_day],
        data[,.(AvgAdmits = mean(N))],fill = T
  )[,factor := AvgAdmits/(.SD[is.na(time_of_day),AvgAdmits])][!is.na(time_of_day)]
}

# Bootstrapping, Confidence Intervals and other Statistical FUNCTIONctions ------------
boot_confInt_target_val <- function(x, FUNCTION = {function(i) mean(i,na.rm = T)}, interval = 95,interval_type = 'basic'){
  boot_name <- switch(interval_type,
                      'basic' = 'basic',
                      'perc' = 'percent',
                      'norm' = 'normal',
                      'stud' = 'student',
                      'bca' = 'bca')
  p <- boot.ci(one.boot(x,FUNCTION,500),conf = interval * 0.01,type = interval_type)[[boot_name]]
  endpoint_indices <- ifelse(rep(interval_type == 'norm',2), c(2,3),c(4,5))
  
  p <- p[endpoint_indices] %>% {function(x) paste0('(',x[1],",",x[2],")")}()
  return(p)
}

boot_confInt_val <-function(x,
                            FUNCTION = { function(i) mean(i, na.rm = T) },
                            interval = 95,
                            mean.only = FALSE,
                            conf.level = NULL,
                            boot.interval.type = 'basic') {
  
  if(length(x) == 0){
    return(0)
  }else if(length(unique(x)) == 1 & length(x) > 10){
    return(paste(x,' (',length(x),')'))
  } else {
    if(is.null(conf.level)){
      conf.level = interval/100
    }
    result <- one.boot(x,FUNCTION = FUNCTION ,R = 1000) %>%  boot.ci(conf = conf.level,type = boot.interval.type)
    if (!is.null(result) & !mean.only){
      
      switch(boot.interval.type,
             'perc' = 'percent',
             'norm' = 'normal')
      
      result <- list(conf.int = result[[boot.interval.type]][,c(4,5)])
      # %>% as.character() %>%
      #   {function(x) paste0('(',x[1],",",x[2],")")}())  %>% unlist() %>% paste0(collapse = ', ')
    } else if (!is.null(result)){
      result <- result$t0
    }
    return(result)
  }
}

boot_confInt_inputs <-
  Vectorize(function(x,
                     FUNCTION,
                     interval = 95,
                     mean.only = FALSE,
                     ci.only = FALSE) {
    if(length(unique(x)) == 1 & length(x) > 10){
      return(paste(x,' (',length(x),')'))
    } else {
      result <- one.boot(x,FUNCTION = FUNCTION ,R = 1000) %>%  boot.ci(conf = interval/100,type = 'basic')
      if(!is.null(result)){
        if (mean.only){
          result <- result$t0
        } else if(ci.only){
          result <- result$basic[c(4,5)]  %>% round(digits = 3) %>% as.character() %>%
            {function(x) paste0('(',x[1],",",x[2],")")}() %>% unlist() %>% paste0(collapse = ', ')
        } else {
          result <- list('t0' = result$t0   %>%  round(digits = 3)  %>% as.character(),
                         int_name = result$basic[c(4,5)]  %>% round(digits = 3) %>% as.character() %>%
                           {function(x) paste0('(',x[1],",",x[2],")")}())  %>% unlist() %>% paste0(collapse = ', ')
        }
        return(result)
      }
    }
  })

dist_fit <- function(data){
  data <- as.numeric(data)
  if(length(na.omit(data)) >= 10){
    dist_type <- names(which.min(mclapply(test_dists(data),function(i) i$aic)))
    fit.estimate <- summary(fitdist(data,dist_type))$estimate
    return(list(parameter_estimates <- fit.estimate))
  }else{
    return(list(mean(data)))
  }
}

dist_cutoff <- function(max_quant,distribution){
  data <- inpatient_stays[inpatient_stays < quantile(inpatient_stays,max_quant)]
  data <- sample(data,3000,replace = F)
  return(fitdist(data,distr = distribution)$loglik)
}

test_dists <- function(values){
  if(any((values %% 1) != 0)){
    dists <- c('norm','lnorm','exp')
    # ,'weibull','beta','gamma','unif')
  }else{
    dists <- c('pois','geom','nbinom','hyper','binom')
  }
  x <- lapply(
    X = dists,
    FUN = function(distribution) {
      tryCatch({
        fitdist(data = values, distr = distribution)
      }, error = function(e) {
        
      })
      
    }
  )
  x <- setNames(x,dists)
}

ci_as_text <- function(interval,dec.digits = 3){
  interval <- specify_decimal(interval,digits = dec.digits)
  paste0("(",min(interval),",",max(interval),")")
}
