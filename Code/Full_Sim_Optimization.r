  unfinished_dir <- file.path('home','adeyemi.n','MH_Simulation','Inpatient Bed Allocation Optimization','Data','full_sim_paused_envr.rdata')
  use_test_bench <- F
  if(dir.exists(unfinished_dir)){
    load(unfinished_dir)
    continue_previous <- T
  } else{
    # Directory to Store MOSA Results -----------------------------------------
    res_dir <- file.path(".","Data","Full Sim Results",gsub('-','_',Sys.Date()))
    if(!dir.exists(res_dir)){
      dir.create(res_dir)
    }
    res_dir <- file.path(res_dir,paste0('Trial_',length(list.files(res_dir))+1))
    dir.create(res_dir)
    source(file.path('Simulations', 'Minnesota MH Network Simulation.R'))
    siteInfo <-
      data.table(readRDS(
        file.path('Simulations', 'Function Requirements', 'Rates5.rds')
      ))
    obj_function_list <-
      grep(pattern = 'mh_',
           x = lsf.str(),
           value = T)
    nVar <- length(siteInfo[,unique(Bed_Group)])
    optim_type = rep('min', 3)
    init_sol <- unlist(sapply(
      X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
      FUN = function(i)
        c(1, rep(x = 0, times = i - 1))))
      warmup <- 30
      sim_length <-  200
      continue_previous <- F
  }