baseline_throughputs <- readRDS(file.path('src','Simulations','baseline-raw-results.rds'))$timestamps[!is.na(Finish.Timestamp),.N,by = replication]

# Function for Mental Health Sim MOSA -------------------------------------
mh_wait_quantile <- function(x){
  if(is.list(x)){
    x <- x$timestamps
  }
  return(x[,.(mh_wait_quantile = quantile(total_wait_time,probs = 0.9,na.rm = T)),by = replication])
}

mh_wait_sigma <- function(x){
  if(is.list(x)){
    x <- x$timestamps
  }  
  return(x[,.(variable = sd(total_wait_time,na.rm = T)^2),by = list(Age,replication)
           ][,.(mh_wait_sigma = max(variable)),by = replication])
}

mh_distance_range <- function(x){
  if(is.list(x)){
    x <- x$timestamps
  }
  return(x[,.(variable = diff(range(Travel.Distance,na.rm = T))),by = list(Age,replication)
           ][,.(mh_distance_range = max(variable)),by = replication])
}

mh_total_throughput <- function(x){

  if(is.list(x)){
    x <- x$timestamps
  }
  
  return(x[!is.na(Finish.Timestamp)][,.(sol_throughput = .N), by = replication][baseline_throughputs[sample(replication,1)][,replication := 1], baseline_throughput :=(N/sim_length), on = .(replication)][, .(mh_total_throughput = sol_throughput/baseline_throughput * 100), by = replication])
}


mh_util_equ <- function(x){
  if(is.list(x)){
    x <- x$resources
  }
  return(x[,`:=`(Occupancy = server/capacity, 
                 lag = data.table::shift(time,type = 'lag',n = 1)), 
           by = list(replication, resource)
           ][siteInfo, classification := i.classification, on = c("resource" = "Bed_Group")
             ][grepl('New Ulm',resource), classification := 'Rural'][time > warmup,
               ][, .(Occupancy = weighted.mean(Occupancy, lag, na.rm = T), 
                     classification = unique(classification)), by = resource
                 ][, mean(Occupancy), by = list(replication,classification)
                   ][, max(classification), by = replication])
  
  
}

  
# Last Updated Spring 2024
MH.Network.sim <-
  function(warm = 10,
           sim_days = 15,
           resources = T,
           seed = NULL,
           ...){

    set.seed(seed)
    add_args = list(...)[[1]]
    
    # Custom Functions Used in the Simulation ---------------------------------------------
    move_environ_name <- function(.env,target_envir){
      assign(x = 'environ_name',value = .env,envir = target_envir)
    }

    obj.name <- function(x) {
      deparse(substitute(x))
    }

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
          get('environ_name', pos = -1) %>% simmer::now() * 3600,
          origin_date = as.POSIXct("2019-1-01 00:00:00", tz = "UTC") - (3600 * 24 * warm)
        )
      curr_weekday <-
        lubridate::wday(curr_time, label = T, abbr = F)
      curr_month <- lubridate::month(curr_time, label = T, abbr = F)
      
      if (arrival_type == 'IP') {
        rate_df <-
          ip_arrival_rates[unit == siteInfo[Age == siteInfo[hospital, Age], ][grepl(pattern = 'Mayo Clinic Hospital - Rochester',
                                                                                    x = Facility_name,
                                                                                    ignore.case = T), Bed_Group], ]
        
        scale_param <- siteInfo[hospital, site_IP_factor]
        adjusted_rate <- prod(rate_df[type == 'Daily', AvgAdmits],
                              # rate_df[grepl(paste(curr_weekday,as.character(timeOfDay(curr_time)),sep = '|'),type), factor],
                              scale_param)
        
      } else {
        adjusted_rate <-  prod(ed_arrival_rates[type == 'Daily', AvgAdmits],
                               # ed_arrival_rates[grepl(paste(curr_weekday, as.character(timeOfDay(curr_time)), sep = '|'), type), factor],
                               hccis[hospital, ed_scale_param],
                               arrival_rate_sa_factor)
        
      }
      x <- rexp(1, adjusted_rate)
      return(x)
    }

    emtala_routing <- function() {
      if(simmer::now(get('environ_name',pos = -1)))
      # Patient Characteristics (age,p(acceptance))

      patient_age <- as.numeric(get_attribute(get('environ_name',pos = -1),"Age"))
      patient_prob <- get_attribute( get('environ_name',pos = -1), 'Acceptance.Prob.Value')
      # patient_priority <- get_prioritization(get('environ_name',pos = -1))
      currED <-   get_attribute(get('environ_name',pos = -1),"Site.Entered")
      currED.name <- gsub('[*]', '', hccis[currED, hccis_id])

      #Establish preferred order (by distance)
      dest <- which.min(sapply(colnames(time.matrix), stringdist, a = currED.name))
      siteInfo.NewOrder <-
        copy(siteInfo)[,`:=`(Drive_Time_hours = time.matrix[dest, Facility_name],
                            Drive_Time_miles = distance.matrix[dest,Facility_name])]
      
      if(any(siteInfo$Facility_name == currED.name)){
        siteInfo.NewOrder <- siteInfo.NewOrder[Facility_name == currED.name, `:=`(Acceptance_Prob = 1)]
      }

      if (sort_by_prob) {
        siteInfo.NewOrder <- siteInfo.NewOrder[order(Acceptance_Prob,decreasing = T),]
      } else {
        siteInfo.NewOrder <- siteInfo.NewOrder[order(Drive_Time_hours, decreasing = F),]
      }
      siteInfo.NewOrder[, `:=`(visit_list = 1, #Initialize list of facilities checked (probability checking)
                               #What sites are allowed to treat patient (by age)
                               age_sites = as.numeric(Age == patient_age),
                               #List What places do not have a queue
                               no_queue = as.numeric(get_capacity( get('environ_name',pos = -1), Bed_Group) -
                                                       get_server_count( get('environ_name',pos = -1), Bed_Group) > 0),
                               # 0 if a patient has previously requested transfer to this facility
                               # prev_accept = get_attribute( get('environ_name',pos = -1), paste0('Prev_Check_Unit_', Site)),
                               ind = seq(.N))]
      # [,`:=`(prev_accept = 1)]
      siteInfo.NewOrder[, `:=`(test_x_accept = Acceptance_Prob * age_sites * no_queue)] #* prev_accepted
      # Vector: NA if cant serve patient, True if transfer request accepted, False if rejected

      siteInfo.NewOrder[, `:=`(referral_response =
                                 fifelse(test = test_x_accept == 0,
                                         yes = NA,
                                         no = patient_prob < test_x_accept
                                 ))]
      #Sample coordination times for all facilities
      siteInfo.NewOrder[, coordination_times := {Vectorize(gen_coord_times)}(Review_Params)]
      siteInfo.NewOrder[Facility_name == currED.name,
                        coordination_times := 0]

      splits = split(x = siteInfo.NewOrder[!is.na(referral_response), Site],
                     f = ceiling(x = seq_along(siteInfo.NewOrder[!is.na(referral_response), Site]) / concurrent_requests))

      siteInfo.NewOrder <- siteInfo.NewOrder[!is.na(referral_response),
                                             round_id := as.numeric(unlist(sapply(X = seq_along(splits),
                                                                                  FUN = function(i) rep(names(splits)[i],
                                                                                                        length(splits[[i]])))))]
      
      if (siteInfo.NewOrder[, .(max(test_x_accept, na.rm = T))] > 0) {

        # List Accepted Transfer Location Information
        if (any(siteInfo.NewOrder[, referral_response], na.rm = T)) {
          # Vectors w/ information of just potential IP units (correct age, no queue, )
          accepted_loc <-
            siteInfo.NewOrder[referral_response == TRUE, ind][1] # First True in accepted_log (closest facility that accepts request)

          if (siteInfo.NewOrder[accepted_loc, round_id] > 1) {

            # Round Coordination time is max of all times in that round if all rejects, o/w it is the coordination time of the first acceptance in that round
            total_coord_time <-
              as.numeric(siteInfo.NewOrder[!is.na(referral_response) &
                                             round_id < siteInfo.NewOrder[accepted_loc, round_id],
                                           .(max_time = max(coordination_times, na.rm = T)), by = round_id
              ][, .(total_coord_time = sum(max_time))])

            n_rejects <-
              siteInfo.NewOrder[referral_response == F &
                                  round_id <= siteInfo.NewOrder[accepted_loc, round_id], .N]

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
                rlnorm(n = 1,
                       meanlog = self_coord_estimates[1],
                       sdlog = self_coord_estimates[2]),
              # Accumulated Coordination time
              drive_distance = siteInfo.NewOrder[accepted_loc, Drive_Time_miles],
              #Travel distance to transfer facility
              num_rejects = n_rejects)

          if (res[1] > length(ip_unit_inds)) {
            get('environ_name',pos = -1) %>% log_("Impossible Unit", level = 1)
          }
        } else{
          # Return value when a patient has too high of an Acceptance Probability but availble beds did exist
          res <-
            c(
              branch_trajectory = 0,
              transfer_site = 0,
              drive_time = 0,
              coordination_time = as.numeric(siteInfo.NewOrder[!is.na(referral_response),
                                                               .(max_time = max(coordination_times, na.rm = T)), by = round_id
              ][, .(total_coord_time = sum(max_time))]),
              drive_distance = 0,
              num_rejects = sum(siteInfo.NewOrder[!is.na(referral_response), .N],
                                get_attribute(get('environ_name',pos = -1),'Rejections'))#,
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
            num_rejects = get_attribute(get('environ_name',pos = -1),'Rejections')
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

    los_duration_fn <- function(curr.time, rate, site) {

      assigned_unit <-
        sample(x = empirical_dist[age ==  get('environ_name',pos = -1) %>%
                                    get_attribute('Age'), location_description_full],
               size = 1,
               replace = T)

      sampled_LoS <- #randomly sample a patient length of stay from Mayo real Length of stays

        empirical_dist[location_description_full == assigned_unit,][sample(.N, 1, replace = T), ip_LoS]

      curr.time <- as.datetime(
        sim_datetime = curr.time * 3600,
        origin_date = as.POSIXct("2019-1-01 00:00:00", tz = "UTC") - (3600 * 24 * warm)
      )
      service_ratio <-
        siteInfo[site, LOS_rate] / (siteInfo[grepl(pattern = "Mayo Clinic Hospital - Rochester",
                                                   x = Facility_name) &
                                               Age ==   get_attribute(get('environ_name',pos = -1),"Age"), LOS_rate])
      service <- service_ratio * sampled_LoS * los_sa_factor
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
        accept.prob <- runif(1)
        res <- accept.prob
      }

      return(res)
    }

    attribute_fn <- function() {

      # Setting the age based on Mayo rates of occurence

        age <-
          sample(
            x = seq(length(age_dist)),
            size =  1,
            replace = TRUE,
            prob = age_dist[order(names(age_dist))]
          )

        # Correct the assigned age if using the national MH ED age distribution

        if (age == 3){
          age <- 4
        } else if (age == 1 || simmer::get_attribute(get('environ_name',pos = -1),'Site.Entered') %in% c(16,17)){
          age <- sample(x = c(1,3), 
                        size = 1,   
                        replace = TRUE, 
                        prob = c(0.9,0.1)
                        )
        }

      request_time <- 0
      tough_barriers <- c("aggresion/violence", "hx.sexual.offense")
      enter.ts <-  get('environ_name',pos = -1) %>%
        simmer::now()

      P.Acceptance <- accept_prob_fun()
      return(
        c(
          Age = age,
          Request.Time = request_time,
          P.Acceptance = P.Acceptance,
          Enter.Timestamp = enter.ts,
          Reject.Time = 0 #,
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
      signal_age <- switch(input,
                           '1' = 'Adolescent',
                           '2' = 'Adult',
                           '3' = 'Pediatric',
                           '4' = 'Geriatric')
      return(unique(siteInfo[grepl(signal_age,Bed_Group,ignore.case = T),free_bed_signal]))
    }

    post_coord_delay <- function(){
      
      # Get actual names of ED and IP facilities 
      ip_dest <- siteInfo[simmer::get_attribute(get('environ_name',pos = -1), 'Transfer.Site'), Facility_name]
      orig_ed <- hccis[simmer::get_attribute(get('environ_name',pos = -1), 'Site.Entered'),hccis_id]
      
      # Only assign a post coordination time if the patient is an internal patient
      if (ip_dest == orig_ed){
        delay <- rexp(1,rate = post_coordination_time[['Internal']]$estimate)
      } else {
        # delay <-  rexp(1,rate = post_coordination_time[['Transfer']]$estimate)
        delay <- 0
      }
      return(delay)
    }

    # Read in Required Data, Inputs, etc. -------------------------------------
    invisible(list2env(readRDS(
      file = file.path("src/Simulations/MH_Network_sim_input_list.rds")
    ), envir = environment()))

    # Modify inputs as necessary
    if (is.list(self_coord_estimates)) {
      self_coord_estimates <- unlist(self_coord_estimates)
    }

    empirical_dist <- empirical_dist[,age := as.numeric(as.factor(age))]
    siteInfo <- siteInfo[!is.na(LOS_rate),][, Site := seq(.N)]

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
      )][hccis, ED.Present := ED_to_IP_Admissions > 0, on = c(Facility_name = 'hccis_id')]

    ed_sites <- which(hccis[, ED_to_IP_Admissions] > 0)
    ed_df <- hccis


    n.fac.unique <- length(unique(siteInfo$Facility_name))
    k <- lapply(split(siteInfo[,.(Ages = list(unique(Age)),Site_num = Site),by = trajectory_num][,trajectory_num := NULL],by = 'Site_num',keep.by = F), unlist, use.names=FALSE)

    sim.length <- (sim_days + warm) * 24
    # Simulation Trajectories ----------------------------------
    # General trajectory for patients transferred to a facility's IP unit
    eval(parse(
      text = paste0("facility.",ip_unit_inds,
                    " <- trajectory('Site.",ip_unit_inds,"') %>%
                    simmer::seize(as.character(siteInfo$Bed_Group[",ip_unit_inds,"])) %>%
                    simmer::set_attribute('bed_seize_timestamp', function() simmer::now(get('environ_name',pos = -1))) %>% 
                    simmer::timeout_from_attribute('Request.Time') %>%
                    simmer::set_attribute('post_coordination_time', function() post_coord_delay()) %>%
                    simmer::timeout_from_attribute('post_coordination_time') %>%
                    simmer::set_attribute('Transfer.Site.Found.Timestamp', function()  simmer::now(get('environ_name',pos = -1))) %>%
                    # simmer::set_attribute(keys = 'total_wait_time', values = function() get_attribute(get('environ_name',pos = -1),'Request.Time'), mod = '+', init =  0) %>%
                    simmer::timeout_from_attribute('Travel.time') %>%
                    simmer::set_attribute('IP.Arrival.Timestamp', function()  simmer::now(get('environ_name',pos = -1))) %>%
                    simmer::set_attribute('ip_LoS',function() los_duration_fn(curr.time = simmer::now(get('environ_name',pos = -1)),rate = siteInfo[",ip_unit_inds,",LOS_rate],site = get_attribute(get('environ_name',pos = -1), 'Transfer.Site'))) %>%
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
                    simmer::set_attribute('ip_LoS', function() los_duration_fn(curr.time = simmer::now(get('environ_name',pos = -1)),rate = siteInfo[",ip_unit_inds,",LOS_rate], site = ",ip_unit_inds,")) %>%
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
        "Rejections" #,
      ), function()
        emtala_routing()) %>%
      simmer::branch(function(){get_attribute(get('environ_name',pos = -1),"Transfer.Trajectory")},
                     continue = FALSE,
                     traj_list)  %>%
      simmer::timeout_from_attribute('Request.Time') %>%
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
          simmer::rollback(amount = 11)
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
                                'Request.Time',
                                'Acceptance.Prob.Value',
                                'Enter.Timestamp',
                                'Rejections'),
                              values = function() attribute_fn()) %>%
        simmer::join(branch.traj)"
      )
    ))

    glob_trajectory <-
      trajectory('glob_traj') %>% set_global(
        keys = function()
          paste0('trap_trip_', seq(4)),
        values = rep(0,4)
      )
    # Parse additional function arguments ------------------------
    if (any(grepl('ed_scale_param', names(add_args)))) {
      
      if(is.data.frame(add_args$ed_scale_param)){
        hccis <-
          hccis[add_args$ed_scale_param, `:=`(tune_scaler = ed_scale_param), on = .(hccis_id)]
        hccis <- hccis[,`:=`(ed_scale_param = ed_scale_param * tune_scaler, tune_scaler = NULL)]
      } else{
        hccis <- hccis[,ed_scale_param := add_args$ed_scale_param*ed_scale_param]
      }
    }
    if (any(grepl('acceptance_prob_input', names(add_args)))) {
      siteInfo <-
        siteInfo[add_args$acceptance_prob_input, Acceptance_Prob := prob, on = .(Facility_name)]
    }
    
    mayo_age_frequency <- age_frequency
    
    if (any(grepl('age_frequency_input', names(add_args)))) {
      age_dist <-
        add_args$age_frequency_input # Setting the patient age based off national rates of occurence
    } else {

      # Follow distribution of arrival ages from Mayo clinin data
      # age_dist <- mayo_age_frequency
      
      # Setting the patient age based off national rates of occurence
      age_dist <- data.table(readxl::read_excel(
        path = file.path(
          "src",
          "Simulations",
          'national_ED_MH_rates.xlsx'),
        sheet = 2
      ))
      age_dist <- age_dist$percentage * 0.01
      names(age_dist) = c('Adolescent','Adult','Geriatric')
    }
    
    # Set the boolean for whether to order potential transfer site by probability
    sort_by_prob <- FALSE
    if(any(grepl('sort_by_prob',names(add_args)))){
      sort_by_prob <- as.logical(add_args$sort_by_prob)
    }

    # Set the number of concurrent transfer requests
    concurrent_requests <- 1
    if(any(grepl('concurrent_requests',names(add_args)))){
      concurrent_requests <- add_args$concurrent_requests
    } 

    # Set the length of stay sensitivity analysis factor
    los_sa_factor <- 1
    if(any(grepl('los_sa_factor',names(add_args)))){
      los_sa_factor <- add_args$los_sa_factor
    } 

    # Set the arrival rate sensitivity analysis factor
    arrival_rate_sa_factor <- 1
    if(any(grepl('arrival_rate_sa_factor',names(add_args)))){
      arrival_rate_sa_factor <- add_args$arrival_rate_sa_factor
    } 
    

    ### Convert a vector of real numbers to a bed allocation ( should be the number of unique bed groups)
    if (any(grepl('input_allocation',names(add_args)))) {
      input_allocation <- add_args$input_allocation
      # Does the supplied solution need to be decoded
      if (all(input_allocation == as.integer(input_allocation))) {
        counts <- input_allocation
      } else{
        counts <-  decode(input_allocation)
      }
      input_allocation <-
        data.table(
          Facility_name = siteInfo$Facility_name[!duplicated(siteInfo$Bed_Group)],
          Bed_Group = siteInfo$Bed_Group[!duplicated(siteInfo$Bed_Group)],
          bed_counts = counts
        )
      siteInfo$total_beds <-
        input_allocation$bed_counts[match(siteInfo$Bed_Group, input_allocation$Bed_Group)]
    }

    # Create the Simulation Environment and Run the Replication --------------
    sim_func_env <- environment()

    create_sim_env <- function(.env = sim_func_env){
      env <- simmer('MH.Network', log_level = 1) %>%
        move_environ_name(target_envir = sim_func_env) %>%
        ed.patient.gen(ed_sites) %>%
        ext.patient.gen(ip_unit_inds) %>%
        resource.creation(ip_unit_inds) %>%
        add_generator(name_prefix = 'global_trap_attr',
                      trajectory = glob_trajectory,
                      at(0),
                      mon = 0)
      return(env)
    }
    
      #If running multiple replications, run in parallel

    raw_results <<- simmer::run(
        .env = create_sim_env(),
        until = sim.length,
        progress = progress::progress_bar$new(format = "[:bar] :percent ETA: :eta")$update
      )
   
    siteInfo <-
      copy(siteInfo)[copy(siteInfo)[, `:=`(N = seq_len(.N)), by = .(Bed_Group)][N == 1, list(ip_unit = Bed_Group, traj_number = Site)],
                     trajectory_num := traj_number, on = c(Bed_Group = 'ip_unit')][, `:=`(trajectory_num = as.numeric(as.factor(trajectory_num)))]
                     

# Retrieve Attributes and Resource DFs ------------------------------------
    timestamps <-
      post_process_arrival_output(
        df = data.table(simmer::get_mon_attributes(raw_results)),
        providers_df = siteInfo,
        ed_df = hccis
      )

    if (resources) {
      resources <-
        post_process_resource_output(
          df = data.table(simmer::get_mon_resources(raw_results)),
          attribute_df = data.table(simmer::get_mon_attributes(raw_results)),
          timestamps_df = timestamps,
          arrivals_df = get_mon_arrivals(raw_results),
          hospital_df = siteInfo
        )

      return(list(timestamps = timestamps,
                  resources = resources))
    } else {
      return(timestamps)
    }
  }
post_process_arrival_output <-
  function(df, providers_df, ed_df) {
    format_attr_df <-  function(i) {
      dcast(data = i[grepl('Transfer', name)
                     ][nchar(name) != 0, `:=`(Sim.ID = paste(name, replication, sep = '_Rep_'))
                       ][!(grepl('Prev_Check_Unit_', key, ignore.case = T)),
                         ][order(list(time, value), decreasing = T)
                           ][, .SD[1,], by = list(Sim.ID, replication, key)],
            formula = Sim.ID + replication ~ key, 
            value.var = 'value')[ed_df[, site_num := as.numeric(rownames(.SD))], ed.nm := hccis_id, on = c(Site.Entered = 'site_num')
                                 ][providers_df, transfer.nm := Facility_name, on = c(Transfer.Site = 'Site')
                                   ][, `:=`(Site.Entered = ed.nm, Transfer.Site = transfer.nm)
                                     ][, `:=`(ed.nm = NULL, transfer.nm  = NULL)
                                       ][, Age := c('Adolescent', 'Adult', 'Child', 'Geriatric')[Age]][, `:=`(
              Age = as.character(Age),
              replication = as.numeric(replication),
              type = ifelse(
                Site.Entered != Transfer.Site |
                  is.na(Transfer.Site),
                'Transfer',
                'Internal'
              )
            )][order(replication, Enter.Timestamp)
               ][, `:=`(total_wait_time = Transfer.Site.Found.Timestamp - Enter.Timestamp)
                 ][providers_df,  `:=`(unit = Bed_Group), on = c(Transfer.Trajectory = 'trajectory_num')]
    }
    if (length(unique(df$replication)) == 1) {
      timestamps = format_attr_df(df)
    } else {
      timestamps <-
        rbindlist(lapply(
          X = split(df, df$replication),
          FUN = format_attr_df,
        ))
    }
    return(timestamps)
  }

post_process_resource_output <-
  function(df, attribute_df, timestamps_df, arrivals_df, hospital_df){

    sub_fun <- function(i) {

      res <- lapply(
        X = unique(hospital_df$Bed_Group),
        FUN = function(test_unit){

            df <- i[resource == test_unit][,cap_change := system - data.table::shift(system,type = 'lag'),by = resource][is.na(cap_change),cap_change := 1]
            external_arrivals <- data.table(arrivals_df)[grepl(pattern = paste(paste0('IP_Unit',unique(hospital_df[Bed_Group == test_unit, Site]),' '),collapse = "|"),name),]
            increases <- df[cap_change > 0,]
            decreases <- df[cap_change < 0,]

            increases <- copy(increases)[resource == test_unit
                                   ][externals[unit == test_unit], `:=`(
                                     patient = `name`,
                                     patient_age = Age,
                                    `ED Patient` = F,
                                    origin = NA_character_,
                                    dest = NA_character_
                                  ), 
                                  on = c('time' = 'IP.Arrival.Timestamp',
                                         'replication' = 'replication'), 
                                  roll = 'nearest'
                                  ][timestamps_df[unit == test_unit], `:=`(
                                                  patient = Sim.ID,
                                                  `ED Patient` = T,
                                                  patient_age = Age,
                                                  origin = Site.Entered,
                                                  dest = Transfer.Site,
                                                  ip_LoS = ip_LoS,
                                                  actual_start = IP.Arrival.Timestamp), 
                                    on = c('time' = 'bed_seize_timestamp', 'replication' = 'replication'), roll = 'nearest'
                                      ][, `:=`(
                                        `External Transfer` = origin != dest,
                                        origin = NULL,
                                        dest = NULL
                                        )][`ED Patient` == T & !is.na(actual_start),time := actual_start
                                               ][,actual_start := NULL
                                                 ][external_arrivals, ip_LoS := activity_time, on = c('patient' = 'name')]

            decreases <- decreases[resource == test_unit][externals[unit == test_unit], `:=`(
              patient = name,
              patient_age = Age,
              `ED Patient` = F,
              origin = NA_character_,
              dest = NA_character_
            ), on = c('time' = 'Finish.Timestamp', 'replication' = 'replication'), roll = 'nearest'
            ][timestamps_df[unit == test_unit], `:=`(
              patient = Sim.ID,
              `ED Patient` = T,
              patient_age = Age,
              ip_LoS = ip_LoS,
              origin = Site.Entered,
              dest = Transfer.Site
            ), on = c('time' = 'Finish.Timestamp', 'replication' = 'replication'), roll = 'nearest'][, `:=`(
              `External Transfer` = origin != dest,
              origin = NULL,
              dest = NULL
            )]

            res <- rbind(increases,decreases, fill = TRUE)[,`:=`(Age = patient_age,
                     patient_age = NULL)]

            return(res)
        })
      res <- rbindlist(res)
      res <- res[order(time)][is.na(`External Transfer`), `External Transfer` := FALSE]
    }
    
  externals <- externals <-  dcast(
          data = attribute_df[grepl('IP_Unit', name), 
          ][nchar(name) != 0, `:=`(Sim.ID = paste(name, replication, sep = '_Rep_'))][, time := NULL],
          formula = name + replication ~ key,
          value.var = 'value'
        )[hospital_df,  `:=`(unit = Bed_Group), on = c(Transfer.Site = 'Site')
        ][ ,Age := c('Adolescent','Adult','Child','Geriatric')[Age]]
  resources <- sub_fun(i=df)[siteInfo[hccis, classification := urban, on= c('Facility_name' = 'hccis_id')], classification := classification, on = c("resource" =  "Bed_Group")]
  return(resources)
  }
