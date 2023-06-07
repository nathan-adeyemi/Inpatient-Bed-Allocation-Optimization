# Run Same Problem w/ NSGA-II Algorithm -----------------------------------
source('.Rprofile')
source(
  file.path(
    "~",
    "MH_Simulation",
    "Inpatient Bed Allocation Optimization",
    "Code",
    "Test_Bed_Opt_Setup.R"
  )
)
res_dir <-
  file.path("Data",
            paste0("Testbench Results (", length(optim_type), " Objectives)"),
            size)
instance_og_dir <- file.path(res_dir,'NSGA_Instances')
loop_min <-
  `if`(dir.exists(instance_og_dir),
       length(list.files(instance_og_dir, pattern = 'Instance_')) + 1,
       1)
for (instance in loop_min:15) {
  instance_dir <-
    file.path(instance_og_dir, paste0('Instance_', instance))
  if(!dir.exists(instance_dir)){
    dir.create(instance_dir)
  }
  now <- Sys.time()
  test_nsga2 <-
    mcnsga2(
      fn = ocbaUpdate,
      varNo = n_queues,
      objDim = length(optim_type),
      lowerBounds = rep(0, n_queues),
      upperBounds = rep(1, n_queues),
      popSize = 50,
      generations = 100,
      cprob = 0.7,
      mprob = 0.2,
      num_replications = 20,
      results_directory = instance_dir
    )
  exec_time <- as.numeric(Sys.time() - now)
  saveRDS(test_nsga2,
          file = file.path(instance_dir, 'NSGA_II_results.rds'))
  dec_env <- environment()
  idx <-
    which(!duplicated(t(
      apply(test_nsga2$parameters, 1, decode, .envir = dec_env)
    )))
  nsga_sols <- test_nsga2$parameters[idx,]
  nsga_pareto_front <-
    lapply(seq_along(idx), function(x)
      update_sol(
        i = x,
        sol_vector = nsga_sols[x,],
        .envir = dec_env
      ))
  nsga_pareto_front <- lapply(
    X = seq_along(nsga_pareto_front),
    FUN = function(i) {
      nsga_pareto_front[[i]]$Obj_mean <-
        test_nsga2$objectives[idx, ][i, ] * c(-1, 1)
      return(nsga_pareto_front[[i]])
    }
  )
  res_list <- list(
    `full_results` = test_nsga2,
    `pareto_front_plot` = plotParetoFront(
      inputData = nsga_pareto_front,
      plot_initial_point = F,
      plot_replications = F,
      plot_ideal_point = F
    ),
    `pareto_front` = nsga_pareto_front,
    `execution_time` = exec_time,
    `percent_correct` = `if`(size != 'Large',
                             pareto_perc_correct(nsga_pareto_front, size = size),
                             0),
    `extra_solns` = `if`(size != 'Large',
                         pareto_perc_correct(nsga_pareto_front, size = size, extras = T),
                         0)
  )
  
  saveRDS(res_list,
          file = file.path(instance_dir, 'NSGA_II_results.rds'))
  par.env <- environment()
  if (instance == 1) {
    instance_df <- with(
      res_list,
      data.table(
        perc_correct = percent_correct,
        extra_solns = extra_solns,
        exec_time = execution_time,
        g_ideal1 = find_g_ideal(pSet = pareto_front, .envir = par.env,dist = F)[1],
        g_ideal_2 = find_g_ideal(pSet = pareto_front, .envir = par.env,dist = F)[2]
      )
    )
  } else{
    if(loop_min > 1){
      instance_df = readRDS(file = file.path(res_dir,paste(size,'Network_NSGA_II_dataframe.rds',sep = '_')))
    }
    instance_df <- rbind(instance_df,
                         with(
                           res_list,
                           data.table(
                             perc_correct = percent_correct,
                             extra_solns = extra_solns,
                             exec_time = execution_time,
                             g_ideal1 = find_g_ideal(pSet = pareto_front, .envir = par.env,dist = F)[1],
                             g_ideal_2 = find_g_ideal(pSet = pareto_front, .envir = par.env,dist = F)[2]
                           )
                         ))
  }
  saveRDS(object = instance_df,
          file = file.path(res_dir,paste(size,'Network_NSGA_II_dataframe.rds',sep = '_')))
}
instance_df <-
  rbind(instance_df,
        instance_df[, lapply(X = .SD, FUN = mean), .SDcols = colnames(instance_df)],
        instance_df[, lapply(
          X = .SD,
          FUN = function(i)
            ci_as_text(interval = t.test(i)$conf.int)
        ), .SDcols = colnames(instance_df)])
saveRDS(instance_df, file = 
          file.path(res_dir,
            paste0(size, '_Network_NSGA_dataframe.rds')
          ))
