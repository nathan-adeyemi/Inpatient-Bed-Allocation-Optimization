# Nathan Adeyemi
# Simulation Code using Simmer Package #
# Last Updated 1/26/2022 #

MH.Network.sim <-
  function(rep = 1,
           warm = 10,
           sim_days = 15,
           n.parallel = 1,
           sort_by_prob = FALSE,
           all_EDs = T,
           resources = T,
           sensitivity_analysis_mult = 1,
           SA_lambda_all = F,
           SA_pr_acceptance = F,
           SA_LoS = F) {
    
    # Custom Functions Used in the Simulation ---------------------------------------------
    move_environ_name <- function(.env,target_envir){
      assign(x = 'environ_name',value = .env,envir = target_envir)
    }
    
    obj.name <- function(x) {
      deparse(substitute(x))
    }
    siteInfo <-
      siteInfo[, Acceptance_Prob := ifelse(test = SA_pr_acceptance, 
                                           yes = sensitivity_analysis_mult, 
                                           no = 1)]
    
    resource.creation <- function(envr, group) {
      eval(parse(
        text = paste0(
          obj.name(envr),
          " %>% add_resource(siteInfo$Bed_Group[",
          group,
          "]%>% as.character(), capacity = siteInfo$total_beds[",
          group,
          "], queue_size = Inf)"
        )
      ))
      return(envr)
    }
    
    ext.patient.gen <- function(envr, group) {
      eval(parse(
        text = paste0(
          obj.name(envr),
          " %>% add_generator('IP_Unit",
          group,
          " External Patient ', S",
          group,
          ".Patient, from_to(0,sim.length+1, dist = function() arr_rate_func(arrival_type = 'IP',hospital = ",
          group,
          "),arrive = FALSE),mon = 2)"
        )
      ))
      return(envr)
    }
    
    
    ed.patient.gen <- function(envr, ed_sites) {
      eval(parse(
        text = paste0(
          obj.name(envr),
          " %>% add_generator(paste0(gsub(pattern = '[*]', replacement = '', ed_df$hccis_id[",
          ed_sites,
          "]),'_Transfer_'), enter.network.S",
          ed_sites,
          ", from_to(0, sim.length+1, dist = function() arr_rate_func(arrival_type = 'ED', hospital =",
          ed_sites,
          "), arrive = FALSE), mon = 2)"
        )
      ))
      return(envr)
    }
    
    timeOfDay <- function(input) {
      if (hour(input) >= 0 & hour(input) < 6) {
        result = 'Early Morning'
      } else if (hour(input) >= 6 & hour(input) < 12) {
        result = 'Morning'
      } else if (hour(input) >= 12 & hour(input) < 18) {
        result = 'Afternoon/Evening'
      } else {
        result = 'Night'
      }
      return(result)
    }
    
    arr_rate_func <- function(arrival_type = NA, hospital) {
      curr_time <-
        simtimer::as.datetime(
           get('environ_name',pos = -1) %>% simmer::now() * 3600,
          origin_date = as.POSIXct("2019-1-01 00:00:00", tz = "UTC") - (3600 * 24 * warm)
        )
      curr_weekday <- lubridate::wday(curr_time, label = T, abbr = F)
      curr_month <- lubridate::month(curr_time, label = T, abbr = F)
      
      if(arrival_type == 'IP'){
        adjusted_rate <-
          mayo_interarrival_rates[unit == arrival_type  & 
                                    location_description == siteInfo[Age == siteInfo[hospital,Age],
                                                                     ][grepl(pattern = 'Mayo Clinic Hospital - Rochester',
                                                                             x = Facility_name,
                                                                             ignore.case = T), Bed_Group] & type == 'Daily', AvgAdmits]
        
      } else {
        adjusted_rate <-
          prod(mayo_interarrival_rates[unit == arrival_type
                                       ][type == as.character(curr_weekday) | 
                                           type == as.character(curr_month) | 
                                           type == as.character(timeOfDay(curr_time)), factor],
               mayo_interarrival_rates[unit == arrival_type &
                                         type == 'Daily', AvgAdmits],
               ifelse(test = SA_lambda_all,
                      yes = sensitivity_analysis_mult,
                      no = 1))
      }
      
      x <-
        rexp(1, prod(
          adjusted_rate,
          ifelse(
            test = arrival_type == 'ED',
            yes = (hccis[hospital, ED.Registrations] /
                     hccis[hccis_id == 'Mayo Clinic Hospital - Rochester', ED.Registrations]),
            no = (siteInfo[hospital, site_IP_factor])
          )
        ))
      return(x)
    }
    
    branch_func <- function() {
      
      # Patient Characteristics (age,p(acceptance))
      patient_age <- as.numeric(get_attribute(get('environ_name',pos = -1),"Age"))
      patient_prob <- get_attribute( get('environ_name',pos = -1), 'Acceptance.Prob.Value')
      currED <-   get_attribute(get('environ_name',pos = -1),"Site.Entered")
      currED.name <- gsub('[*]', '', hccis[currED, hccis_id])
      
      #Establish preferred order (by distance)
      siteInfo.NewOrder <-
        copy(siteInfo)[data.table(name = rownames(distance.matrix),
                                  drive.time = time.matrix[, currED.name],
                                  drive.dist = distance.matrix[, currED.name]),
                       `:=`(Drive_Time_hours = drive.time,
                            Drive_Time_miles = drive.dist),on = c(Facility_name = 'name')][order(Drive_Time_hours, decreasing = F),]
      siteInfo.NewOrder[, `:=`(visit_list = 1, #Initialize list of facilities checked (probability checking)
                                             #What sites are allowed to treat patient (by age)
                               age_sites = as.numeric(Age == patient_age),
                               #List What places do not have a queue
                               no_queue = as.numeric(get_capacity( get('environ_name',pos = -1), Bed_Group) - 
                                                       get_server_count( get('environ_name',pos = -1), Bed_Group) > 0),
                               # 0 if a patient has previously requested transfer to this facility
                               # prev_accept = get_attribute( get('environ_name',pos = -1), paste0('Prev_Check_Unit_', Site)),
                               ind = seq(.N))]
      siteInfo.NewOrder[Facility_name == currED.name, `:=`(Acceptance_Prob = 1)
                        ]# [,`:=`(prev_accept = 1)]
      siteInfo.NewOrder[, `:=`(test_x_accept = Acceptance_Prob * age_sites * no_queue)] #* prev_accepted
      # Vector: NA if cant serve patient, True if transfer request accepted, False if rejected
      siteInfo.NewOrder[, `:=`(referral_response = 
                                 fifelse(test = test_x_accept == 0,
                                         yes = NA,
                                         no = patient_prob < test_x_accept))]
                        #Sample coordination times for all facilities
      siteInfo.NewOrder[, coordination_times := {Vectorize(gen_coord_times)}(Review_Params)]
      siteInfo.NewOrder[Facility_name == currED.name, 
                        coordination_times := 0]
      splits = split(x = siteInfo.NewOrder[!is.na(referral_response), Site],
                     f = ceiling(x = seq_along(siteInfo.NewOrder[!is.na(referral_response), Site]) / n.parallel))
      siteInfo.NewOrder <- siteInfo.NewOrder[!is.na(referral_response), 
                                             round_id := as.numeric(unlist(sapply(X = seq_along(splits),
                                                                       FUN = function(i) rep(names(splits)[i],
                                                                                             length(splits[[i]])))))]
      if (sort_by_prob) {
        siteInfo.NewOrder <- siteInfo.NewOrder[order(Acceptance_Prob,decreasing = T),]
      }
      if (siteInfo.NewOrder[, .(max(test_x_accept, na.rm = T))] > 0) {
      
        # List Accepted Transfer Location Information
        if (any(siteInfo.NewOrder[, referral_response], na.rm = T)) {
          # Vectors w/ information of just potential IP units (correct age, no queue, )
          accepted_loc <-
            siteInfo.NewOrder[referral_response == TRUE, ind][1] # First True in accepted_log (closest facility that accepts request)
          # siteInfo.NewOrder[ind < accepted_loc &
          #                     !is.na(referral_response), prev_accept := 0]
          
          if (siteInfo.NewOrder[accepted_loc, round_id] > 1) {
            # siteInfo.NewOrder[!is.na(referral_response) &
            #                     round_id < siteInfo.NewOrder[accepted_loc,round_id], prev_accept := 1]
            
            # Round Coordination time is max of all times in that round if all rejects, o/w it is the coordination time of the first acceptance in that round
            total_coord_time <-
              as.numeric(siteInfo.NewOrder[!is.na(referral_response) &
                                  round_id < siteInfo.NewOrder[accepted_loc, round_id],
                                .(max_time = max(coordination_times, na.rm = T)), by = round_id
                                ][, .(total_coord_time = sum(max_time))])
        
            n_rejects <- 
              siteInfo.NewOrder[referral_response == F &
                                  round_id <= siteInfo.NewOrder[accepted_loc, round_id], .N]
            

            # sum(siteInfo.NewOrder[accepted_loc, coordination_times],
            #     rlnorm(n = n_rejects,
            #            meanlog =  buffers_params[1],
            #            sdlog =  buffers_params[2])) 
          } else {
            total_coord_time <-
              siteInfo.NewOrder[accepted_loc, coordination_times]
            n_rejects = 0
          }

          res <-
            c(
              branch_trajectory = siteInfo.NewOrder[accepted_loc, trajectory_num],
              transfer_site = siteInfo.NewOrder[accepted_loc, Site],
              # Site number of transfer facility
              drive_time = siteInfo.NewOrder[accepted_loc, Drive_Time_hours],
              # Travel time to transfer facility
              coordination_time =  total_coord_time <-
                total_coord_time + 
                # ifelse(
                #   test = siteInfo.NewOrder[accepted_loc, Facility_name] == currED.name,
                #   yes = rexp(1, rate = bed_prep_params[['estimate']]),
                #   no = 0
                # ),
                rlnorm(n = 1,
                       meanlog = self_coord_estimates[1],
                       sdlog = self_coord_estimates[2]),
              # Accumulated Coordination time
              drive_distance = siteInfo.NewOrder[accepted_loc, Drive_Time_miles],
              #Travel distance to transfer facility
              num_rejects = n_rejects # +   get_attribute(get('environ_name',pos = -1),'Times.Rejected') #,
              # prev_accept = siteInfo.NewOrder[order(Site), prev_accept]
            )
          
          if (res[1] > length(ip_unit_inds)) {
             get('environ_name',pos = -1) %>% log_("Impossible Unit", level = 1)
          }
        } else{
          # Return value when a patient has too high of an Acceptance Probability but availble beds did exist
          # siteInfo.NewOrder[!is.na(referral_response), prev_accept := 0]
          res <-
            c(
              branch_trajectory = 0,
              transfer_site = 0,
              drive_time = 0,
              # coordination_time = sum(siteInfo.NewOrder[!is.na(referral_response),
              #                                       .(max_time = max(coordination_times, na.rm = T)), by = round_id
              #                                       ][, .(total_coord_time = sum(max_time))],
              #                         rlnorm(n = max(siteInfo.NewOrder[, round_id], na.rm = T) - 1,
              #                                meanlog = buffers_params[1],
              #                                sdlog = buffers_params[2])),
              coordination_time = as.numeric(siteInfo.NewOrder[!is.na(referral_response),
                                                        .(max_time = max(coordination_times, na.rm = T)), by = round_id
                                                        ][, .(total_coord_time = sum(max_time))]),
              drive_distance = 0,
              num_rejects = sum(siteInfo.NewOrder[!is.na(referral_response), .N],
                                  get_attribute(get('environ_name',pos = -1),'Times.Rejected'))#,
              # prev_accept = siteInfo.NewOrder[order(Site), prev_accept]
              )
        }
      } else {
        res <-
          c(
            branch_trajectory = 0,
            transfer_site = 0,
            drive_time = 0,
            coordination_time = 0,
            drive_distance = 0,
            num_rejects = get_attribute(get('environ_name',pos = -1),'Times.Rejected')#,
            # prev_accept = siteInfo.NewOrder[order(Site), prev_accept]
          )
      }
      return(res)
  }
    
    getDay <- function(ref_date, previous_fri = T) {
      if (previous_fri == T) {
        dates <- seq(
          from = (as.Date(ref_date) - 7),
          to = (as.Date(ref_date) - 1),
          by = "days"
        )
        dates[lubridate::wday(dates, label = T) == 'Fri']
      } else {
        dates <- seq(from = as.Date(ref_date),
                     to = (as.Date(ref_date) + 7),
                     by = "days")
        dates[lubridate::wday(dates, label = T) == 'Mon']
      }
    }
    
    wait.function <- function(curr.time, rate, site) {
      
      assigned_unit <-
        sample(x = empirical_dist[age ==  get('environ_name',pos = -1) %>%
                                    get_attribute('Age'), location_description],
               size = 1,
               replace = T)
      
      sampled_LoS <- #randomly sample a patient length of stay from Mayo real Length of stays
        
        prod(empirical_dist[location_description == assigned_unit,
        #empirical_dist[age ==   get_attribute(get('environ_name',pos = -1),'Age'),
                       ][sample(.N, 1, replace = T), ip_LoS],
              ifelse(test = SA_LoS,sensitivity_analysis_mult,1))

      curr.time <- as.datetime(
        sim_datetime = curr.time * 3600,
        origin_date = as.POSIXct("2019-1-01 00:00:00", tz = "UTC") - (3600 * 24 * warm)
      )
      service_ratio <-
        siteInfo[site, LOS_rate] / (siteInfo[grepl(pattern = "Mayo Clinic Hospital - Rochester", 
                                                   x = Facility_name) & 
                                               Age ==   get_attribute(get('environ_name',pos = -1),"Age"), LOS_rate])
      service <- service_ratio * sampled_LoS
      # Shift the inpatient LoS to the nearest release time
      expected_discharge <-
        curr.time + (service * 3600)
      
      friday_discharge <-
        as.POSIXct(paste(getDay(expected_discharge), "00:22:00"))
      
      monday_discharge <-
        as.POSIXct(paste((as.Date(friday_discharge) + 3), "00:06:00"))
      
      wait <- 0
      
      if (((expected_discharge > friday_discharge) &
           (expected_discharge < monday_discharge)) &
          !((curr.time > friday_discharge) &
            (curr.time < monday_discharge))){
        
        wait <- as.numeric(difftime(
          time1 = c(friday_discharge, monday_discharge),
          time2 = expected_discharge,
          units = 'hours'
        ))
        wait <- wait[which.min(abs(wait))]
      }
      # return(service + wait)
      return(service)
    }
    
    accept_prob_fun <- function(prev_prob = NA) {
      if (!is.na(prev_prob)) {
        res <-
          rtri(
            n = 1,
            min = 0,
            max = max(prev_prob, 0.5),
            mode = max(0.8 * prev_prob, 0.25)
          )
      } else {
        # accept.prob <- rtri(1,0,1,mode = (esi/(max_esi+1)))
        accept.prob <- runif(1)
        res <- accept.prob
      }
      
      return(res)
    }
    
    attribute.func <- function() {
      age <-
        sample(
          x = seq(4),
          size =  1,
          replace = TRUE,
          prob = age_frequency[order(names(age_frequency))]
        )
      
      esi <-
        sample(
          x = seq(length(esi_frequency)),
          size = 1,
          replace = T,
          prob = esi_frequency[order(names(esi_frequency))]
        )
      
      request_time <- 0
      tough_barriers <- c("aggresion/violence", "hx.sexual.offense")
      enter.ts <-  get('environ_name',pos = -1) %>% 
        simmer::now()
      
      P.Acceptance <- accept_prob_fun()
      return(
        c(
          Age = age,
          esi = esi,
          Request.Time = request_time,
          P.Acceptance = P.Acceptance,
          Enter.Timestamp = enter.ts,
          Reject.Time = 0 #,
          # rep(1,length(siteInfo[,Site]))
        )
      )
      
    }
    
    get_trajectory_names <- function(envr) {
      traj_list_names <- ls(pattern = "facility", envir = envr)
      traj_list_names <-
        traj_list_names[mixedorder(substr(traj_list_names, 10, ifelse(nchar(
          traj_list_names
        ) == 10, 10, 11)))]
      return(traj_list_names)
    }
    
    check_ip_occupancy <- function(){
      units <- siteInfo[Age == get_attribute( get('environ_name',pos = -1),'Age'),Bed_Group]
      caps <- get_capacity( get('environ_name',pos = -1),units)
      server_counts <- get_server_count( get('environ_name',pos = -1),units)
      # print(get_name( get('environ_name',pos = -1)))
      # debug(branch_func)
      return(all(sapply(seq(length(units)),function(i) caps[i] == server_counts[i])))
    }
    
    gen_coord_times <- function(dt_col) {
      return(ifelse(
        test = length(unlist(dt_col)) == 1,
        yes = rexp(1, unlist(dt_col)),
        no = rlnorm(
          n = 1,
          meanlog = unlist(dt_col)[1],
          sdlog = unlist(dt_col)[2]
        )
      ))
    }
    
    applicable_signals <- function(input){
      #signal_age <- switch(get_attribute( get('environ_name',pos = -1),'Age'),
      signal_age <- switch(input,                     
                           '1' = 'Adolescent',
                           '2' = 'Adult',
                           '3' = 'Pediatric',
                           '4' = 'Geriatric')
      return(unique(siteInfo[grepl(signal_age,Bed_Group,ignore.case = T),free_bed_signal]))
    }
    
    
    # Read in Required Data, Inputs, etc. -------------------------------------
    list2env(readRDS(
      file = file.path(
        ".",
        "Data",
        "Function Requirements",
        "MH_Network_sim_input_list.rds"
      )
    ), envir = environment())
    
    # Modify inputs as necessary
    if (is.list(self_coord_estimates)) {
      self_coord_estimates <- unlist(self_coord_estimates)
    }
    
    empirical_dist <- empirical_dist[,age := as.numeric(as.factor(age))]
    siteInfo <- siteInfo[!is.na(LOS_rate),][, Site := seq(.N)]
    # buffers_params <- unlist(buffers_params)
    # buffers_params <-
    #   suppressWarnings(as.numeric(buffers_params[!is.na(as.numeric(buffers_params))]))
    
    # Simulation Initial Conditions -------------------------------------------
    # Set default function argument values
    ip_unit_inds <- siteInfo[, `:=`(N=seq_len(.N)), by=.(Bed_Group)][N == 1, list(ip_unit = Bed_Group,traj_number = Site)]
    siteInfo <- siteInfo[ip_unit_inds,trajectory_num := traj_number, on = c(Bed_Group = 'ip_unit')
                         ][,`:=`(N = NULL,trajectory_num = as.numeric(as.factor(trajectory_num)))
                           ][,free_bed_signal := paste0(Bed_Group,' bed available for placement')]
    ip_unit_inds <- ip_unit_inds[,traj_number]
    
    siteInfo <-
      siteInfo[, `:=`(
        total_beds = as.numeric(total_beds),
        Bed_Group = as.character(Bed_Group),
        Age = as.numeric(as.factor(Age)),
        Facility_name = as.character(Facility_name)
      )][hccis, ED.Present := ED.Registrations > 0, on = c(Facility_name = 'hccis_id')]
    
    if (all_EDs) {
      ed_sites <- which(hccis[, ED.Registrations] > 0)
      ed_df <- hccis
    } else {
      ed_df <- siteInfo
      siteInfo[hccis, ED.Registrations := ED.Registrations, on = c(Facility_name = 'hccis_id')]
      ed_sites <- which(siteInfo[, ED.Registrations] > 0)
    }
    
    n.fac.unique <- length(unique(siteInfo$Facility_name))
    k <- lapply(split(siteInfo[,.(Ages = list(unique(Age)),Site_num = Site),by = trajectory_num][,trajectory_num := NULL],by = 'Site_num',keep.by = F), unlist, use.names=FALSE)
    
    # barr_names <- gsub(pattern = ' ',replacement = '.',x = barrierRates[[2]][,V1])
    sim.length <- (sim_days + warm) * 24
    # Simulation Trajectories ----------------------------------
    # General trajectory for patients transferred to a facility's IP unit
    eval(parse(
      text = paste0("facility.",ip_unit_inds,
                    " <- trajectory('Site.",ip_unit_inds,"') %>% 
                    simmer::seize(as.character(siteInfo$Bed_Group[",ip_unit_inds,"])) %>% 
                    simmer::set_attribute('Transfer.Site.Found.Timestamp', function()  simmer::now(get('environ_name',pos = -1))) %>%
                    simmer::timeout_from_attribute('Request.Time') %>%
                    # simmer::set_attribute(keys = 'total_wait_time', values = function() get_attribute(get('environ_name',pos = -1),'Request.Time'), mod = '+', init =  0) %>% 
                    simmer::timeout_from_attribute('Travel.time') %>%
                    simmer::set_attribute('IP.Arrival.Timestamp', function()  simmer::now(get('environ_name',pos = -1))) %>% 
                    simmer::set_attribute('ip_LoS',function() wait.function(curr.time = simmer::now(get('environ_name',pos = -1)),rate = siteInfo[",ip_unit_inds,",LOS_rate],site = get_attribute(get('environ_name',pos = -1), 'Transfer.Site'))) %>% 
                    simmer::timeout_from_attribute('ip_LoS') %>% 
                    simmer::release(as.character(siteInfo[", ip_unit_inds, ",Bed_Group])) %>%
                    simmer::send(siteInfo[", ip_unit_inds, ",free_bed_signal]) %>% 
                    simmer::set_global(keys = paste0('trap_trip_', k[[as.character(",ip_unit_inds,")]]),values = rep(0,length(k[[as.character(",ip_unit_inds,")]]))) %>% 
                    simmer::set_attribute('Finish.Timestamp', function() simmer::now(get('environ_name',pos = -1)))")
    ))
    
    # General trajectory for patients (non-ED) placed in a facility's IP bed
    eval(parse(
      text = paste0("S", ip_unit_inds, ".Patient <- trajectory('Ext.Site.",ip_unit_inds,"') %>% 
                    simmer::set_attribute('Transfer.Site',as.numeric(", ip_unit_inds,")) %>% 
                    simmer::set_attribute('Age',siteInfo[",ip_unit_inds, ",Age]) %>% 
                    simmer::set_attribute('IP.Arrival.Timestamp',function() simmer::now(get('environ_name',pos = -1))) %>%
                    simmer::seize(as.character(siteInfo[",ip_unit_inds,",Bed_Group])) %>% 
                    simmer::set_attribute('ip_LoS', function() wait.function(curr.time = simmer::now(get('environ_name',pos = -1)),rate = siteInfo[",ip_unit_inds,",LOS_rate], site = ",ip_unit_inds,")) %>% 
                    simmer::timeout_from_attribute('ip_LoS') %>% 
                    simmer::release(as.character(siteInfo[", ip_unit_inds,",Bed_Group])) %>% 
                    simmer::send(siteInfo[", ip_unit_inds, ",free_bed_signal]) %>% 
                    simmer::set_global(keys = paste0('trap_trip_', k[[as.character(",ip_unit_inds,")]]),values = rep(0,length(k[[as.character(",ip_unit_inds,")]]))) %>%
                    simmer::set_attribute('Finish.Timestamp', function() simmer::now(get('environ_name',pos = -1)))")
    ))
    
    # Gets names of all trajectories for branching trajectory later on
    traj_list <-
      mget(get_trajectory_names(envr = environment()), envir = environment())
    
    # Trajectory determining where patients are placed (first time).
    # Calls the branch_function for actual placement
    branch.traj <-
      simmer::trajectory('branch.traj') %>%
      simmer::set_attribute('rolled.back',0) %>%
      # simmer::set_attribute('total_wait_time',0) %>% 
      simmer::set_attribute(c(
        "Transfer.Trajectory",
        "Transfer.Site",
        "Travel.time",
        "Request.Time",
        "Travel.Distance",
        "Times.Rejected" #,
        # paste0('Prev_Check_Unit_',siteInfo[,Site])
      ), function()
        branch_func()) %>%
      simmer::branch(function(){get_attribute(get('environ_name',pos = -1),"Transfer.Trajectory")},
        continue = FALSE,
        traj_list)  %>%
      simmer::timeout_from_attribute('Request.Time') %>%
      # simmer::set_attribute('total_wait_time',
      #               values = function() get_attribute(get('environ_name',pos = -1),'Request.Time'),
      #               mod = "+",
      #               init =  0) %>%
      simmer::trap(signals = function() applicable_signals(get_attribute( get('environ_name',pos = -1),'Age'))) %>%
      simmer::wait() %>%
      simmer::timeout(task = function() rexp(1,5)) %>%
      simmer::branch(
        option = function(x) {
          res <-
            as.numeric(get_global(
              get('environ_name', pos = -1),
              keys = paste0('trap_trip_', get_attribute(get(
                'environ_name', pos = -1
              ), 'Age'))
            ) == 0)
          return(res)
        },
        
        continue = FALSE,
        trajectory() %>%
          simmer::set_global(
            keys = function()
              paste0('trap_trip_', get_attribute(get(
                'environ_name', pos = -1
              ), 'Age')),
            values = 1,
            mod = '+',
            init = 0
          ) %>%
          # simmer::set_attribute(
          #   'release_time',
          #   values = function()
          #     now(get('environ_name', pos = -1))) %>%
          simmer::set_attribute("rolled.back",
                                values = 1,
                                mod = "+") %>%
          simmer::set_attribute("Acceptance.Prob.Value",
                                function()
                                  accept_prob_fun(prev_prob = get_attribute(
                                    get('environ_name', pos = -1), "Acceptance.Prob.Value"
                                  ))) %>%
          simmer::untrap(
            signals = function()
              applicable_signals(get_attribute(get(
                'environ_name', pos = -1
              ), 'Age'))
          ) %>%
          simmer::rollback(amount = 10)
      ) %>%
      simmer::rollback(amount = 3)

    # General Trajectory for patient who arrive at a facility's ED
    eval(parse(
      text = paste0(
        "enter.network.S",
        ed_sites,
        " <- simmer::trajectory('Patient.path') %>% 
        simmer::set_attribute('Site.Entered',",ed_sites,") %>% 
        simmer::set_attribute(keys = c('Age',
                                'ESI',
                                'Request.Time',
                                'Acceptance.Prob.Value',
                                'Enter.Timestamp',
                                'Times.Rejected' #,
                                # paste0('Prev_Check_Unit_',siteInfo[,Site])
                                ), 
                              values = function() attribute.func()) %>%
        simmer::join(branch.traj)"
      )
    ))
    
    glob_trajectory <-
      trajectory('glob_traj') %>% set_global(
        keys = function()
          paste0('trap_trip_', seq(4)),
        values = rep(0,4)
      )

    # Create the Simulation Environment and Run the Replication --------------
    sim_func_env <- environment()
    if (length(rep) == 1) {
      #If running multiple replications, run in parallel
      sim_results <- simmer('MH.Network', log_level = 1) %>%
        move_environ_name(target_envir = sim_func_env) %>%
        ed.patient.gen(ed_sites) %>%
        ext.patient.gen(ip_unit_inds) %>%
        resource.creation(ip_unit_inds) %>%
        add_generator(name_prefix = 'global_trap_attr',
                      trajectory = glob_trajectory,
                      at(0),
                      mon = 0) %>%
        simmer::run(until = sim.length,
                    progress = progress::progress_bar$new(format = "[:bar] :percent ETA: :eta")$update)
    } else {
      sim_results <- pbmclapply(
        X = rep,
        FUN = function(i)
          simmer('MH.Network', log_level = 1) %>%
          move_environ_name(target_envir = sim_func_env) %>%
          ed.patient.gen(ed_sites) %>%
          ext.patient.gen(ip_unit_inds) %>%
          resource.creation(ip_unit_inds) %>%
          simmer::run(
            until = sim.length
          ) %>%
          wrap(),
        mc.cores = ifelse(
          test = length(rep) == 1,
          yes = 1,
          no = availableCores()
        ),
        mc.set.seed = T
      )
    }
    
    siteInfo <- copy(siteInfo)[copy(siteInfo)[, `:=`(N = seq_len(.N)), by = .(Bed_Group)
                                              ][N == 1, list(ip_unit = Bed_Group, traj_number = Site)],
                               trajectory_num := traj_number, on = c(Bed_Group = 'ip_unit')
                               ][, `:=`(trajectory_num = as.numeric(as.factor(trajectory_num)))]

    # Retrieve Attributes and Resource DFs ------------------------------------
    attributes <- data.table(simmer::get_mon_attributes(sim_results))
    timestamps <- 
      rbindlist(
      pbmclapply(X = split(attributes,attributes$replication),
               FUN = function(i){
                 dcast(i[grepl('Transfer', name)
                                ][nchar(name) != 0, `:=`(Sim.ID = paste(name, replication, sep = '_Rep_'))
                                  ][!(grepl('Prev_Check_Unit_',key,ignore.case = T)),
                                    ][order(list(time,value),decreasing = T)
                                      ][,.SD[1,], by = list(Sim.ID,replication,key)], 
                        Sim.ID + replication ~ key, value.var = 'value'
                       )[hccis[,site_num := as.numeric(rownames(.SD))],ed.nm := hccis_id, on = c(Site.Entered = 'site_num')
                          ][siteInfo, transfer.nm := Facility_name, on = c(Transfer.Site = 'Site')
                            ][,`:=`(Site.Entered = ed.nm, Transfer.Site = transfer.nm)
                              ][,`:=`(ed.nm = NULL, transfer.nm  = NULL)
                                ][,Age := c('Adolescent', 'Adult', 'Child', 'Geriatric')[Age]
                                  ][,`:=`(Age = as.factor(Age),
                                          replication = as.numeric(replication),
                                          type = ifelse(Site.Entered != Transfer.Site | 
                                                          is.na(Transfer.Site),'Transfer','Internal'))
                                    ][order(replication,Enter.Timestamp)
                                        ][Enter.Timestamp >= warm * 24,
                                          ][,Transfer.Site.Found.Timestamp := Transfer.Site.Found.Timestamp + Request.Time
                                            ][,total_wait_time := Transfer.Site.Found.Timestamp - Enter.Timestamp]
                 },
               mc.cores = ifelse(test = length(rep) != 1, yes = availableCores(), no = 1))
      )
    if (resources) {
      
      externals <- 
        rbindlist(mclapply(X = split(attributes,attributes$replication),
                 FUN = function(i){
                   dcast(data = i[grepl('IP_Unit', name),]
                                  [nchar(name) != 0, `:=`(Sim.ID = paste(name, replication, sep = '_Rep_'))
                                    ][, time := NULL],
                         formula = name + replication ~ key,
                         value.var = 'value')
                 }))
      resources <-
        data.table(simmer::get_mon_resources(sim_results))
      
      resources <- 
        rbindlist(
          mclapply(X = split(resources,resources$replication),
                 FUN = function(i){
                   i[externals[siteInfo, `:=`(unit = Bed_Group), on = c(Transfer.Site = 'Site')], 
                     `:=`(patient = name, 
                       `ED Patient` = F, 
                       `External Transfer` = F),
                     on = c(time = 'IP.Arrival.Timestamp',
                         resource = 'unit',
                         replication = 'replication')
                      ][externals[siteInfo,  `:=`(unit = Bed_Group), on = c(Transfer.Site = 'Site')], 
                        `:=`(patient = name,
                             `ED Patient` = F, 
                             `External Transfer` = F),
                        on = c(time = 'Finish.Timestamp',
                               resource = 'unit',
                               replication = 'replication')
                        ][timestamps[siteInfo,  `:=`(unit = Bed_Group), on = c(Transfer.Trajectory = 'trajectory_num')],
                          `:=`(patient = Sim.ID, 
                               `ED Patient` = T,
                               `External Transfer` = (type == 'Transfer'),
                               actual_ip_start = IP.Arrival.Timestamp,
                               start = T),
                          on = c(time = 'Transfer.Site.Found.Timestamp',
                                 resource = 'unit',
                                 replication = 'replication')
                          ][timestamps[siteInfo,  `:=`(unit = Bed_Group), on = c(Transfer.Trajectory = 'trajectory_num')],
                            `:=`(patient = Sim.ID,
                                 `ED Patient` = T,
                                 `External Transfer` = (type == 'Transfer')),
                            on = c(time = 'Finish.Timestamp',
                                   resource = 'unit',
                                   replication = 'replication')
                            ][order(replication,time)
                              ][time >= warm * 24,
                                ][is.na(actual_ip_start),actual_ip_start := time]
                   
                 },
                 mc.cores = ifelse(test = length(rep) != 1, yes = availableCores(), no = 1))
          )
      
      return(list(timestamps = timestamps, 
                  resources = resources))
    } else {
      
      return(timestamps)
    }
  }

full_sim <-
  function(num_iter = 12,
           parallel = TRUE,
           concurrent_requests = NULL,
           prob_sort = F,
           new_sol = NULL,
           warmup = 50,
           sim_length = 365,
           seed = NULL,
           save_files = T,
           return_resources = T,
           multipliers = 1,
           all_lambda_vary = F,
           pr_accept_vary = F,
           los_vary = F,
           vuln_lambda = F){
    
    if(los_vary){
      insert = 'los'
    } else if(all_lambda_vary){
      insert = 'arr_rate'
    } else{
      insert = 'pr_accept'
    }
    
    temp_path = path = file.path(
      '.',
      'Simulation and Alternatives',
      'Sensitivity Analysis Results',
      paste0(insert, '_temp_folder')
    )

    # Run actual simulation on clusters from above
    runs <-
      data.table(expand_grid(
        replications = seq(num_iter),
        factors = multipliers
      ))
    
    if (length(list.files(temp_path)) != 0) {
      last_run <-
        max(as.numeric(gsub(
          pattern = 'temp_|.rds',
          replacement = '',
          x = list.files(temp_path)
        )))
      runs <- runs[(last_run+1):nrow(runs)]
    }
      
    results <- mclapply(
      X = seq(nrow(runs)),
      FUN = function(index) {
        l <- MH.Network.sim(
          warm = warmup,
          sim_days = sim_length,
          sort_by_prob = prob_sort,
          sensitivity_analysis_mult = runs[index, factors],
          n.parallel = 1,
          resources = F,
          SA_lambda_all = all_lambda_vary,
          SA_pr_acceptance = pr_accept_vary,
          SA_LoS = los_vary
        )[, `:=`(replication = runs[index, replications], factor = runs[index, factors])]
        saveRDS(
          l,
          file = file.path(temp_path,
            paste0('temp_', index, '.rds')
          )
        )
        return(l)
      },
      mc.cores = availableCores()
    )
    return(results)
  }
