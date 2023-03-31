# Run Same Problem w/ NSGA-II Algorithm -----------------------------------
<<<<<<< HEAD
<<<<<<< HEAD
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
for (instance in seq(15)) {
  instance_dir <-
    file.path(res_dir, paste0('NSGA_Instance_', instance))
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
    `percent_correct` = pareto_perc_correct(nsga_pareto_front, size = size),
    `extra_solns` = pareto_perc_correct(nsga_pareto_front, size = size, extras = T)
  )
  
  saveRDS(res_list,
          file = file.path(instance_dir, 'NSGA_II_results.rds'))
  
  if (instance == 1) {
    instance_df <- with(
      res_list,
      data.table(
        perc_correct = percent_correct,
        extra_solns = extra_solns,
        exec_time = execution_time,
        g_ideal1 = apply(
          find_g_ideal(pSet = nsga_pareto_front, .envir = par.env),
          2,
          mean
        )[1],
        g_ideal_2 = apply(
          find_g_ideal(pSet = nsga_pareto_front, .envir = par.env),
          2,
          mean
        )[2]
      )
    )
  } else{
    instance_df <- rbind(instance_df,
                         with(
                           res_list,
                           data.table(
                             perc_correct = percent_correct,
                             extra_solns = extra_solns,
                             exec_time = execution_time,
                             g_ideal1 = apply(
                               find_g_ideal(pSet = nsga_pareto_front, .envir = par.env),
                               2,
                               mean
                             )[1],
                             g_ideal_2 = apply(
                               find_g_ideal(pSet = nsga_pareto_front, .envir = par.env),
                               2,
                               mean
                             )[2]
                           )
                         ))
  }
}
instance_df <-
  rbind(instance_df,
        instance_df[, lapply(X = .SD, FUN = mean), .SDcols = colnames(instance_df)],
        instance_df[, lapply(
          X = .SD,
          FUN = function(i)
            ci_as_text(interval = t.test(i)$conf.int)
        ), .SDcols = colnames(med_df)])
saveRDS(instance_df, file = 
          file.path(res_dir,
            paste0(size, '_Network_NSGA_dataframe.rds')
          ))
=======
nsgaCostFn <- function(x) {
return(CostFunction(sol = x,
                logic = F,
                nsga = T))
}
=======
>>>>>>> 07d1520 (1. New File -  "Test_Bed_Opt_Setup.R": automates setting up the jackson network test simulation environment and relevant parameters.)
test_nsga2 <-
  mcnsga2(
    fn = nsgaCostFn,
    varNo = nVar,
    objDim = length(optim_type),
    lowerBounds = rep(0, nVar),
    upperBounds = rep(, nVar),
    popSize = 40,
    generations = 100,
    cprob = 0.7,
    mprob = 0.2
  )

saveRDS(test_nsga2,
        file = file.path(res_dir, 'NSGA_II_results.rds'))
idx <-
  apply(
    X = unique(t(
      apply(test_nsga2$parameters, 1, decode, test_bench = T)
    )),
    MARGIN = 1,
    FUN = function(a)
      apply(t(
        apply(test_nsga2$parameters, 1, decode, test_bench = T)
      ), 1, function(b)
        all(a == b))
  )
idx <- which(!duplicated(apply(idx, 1, which)))
nsga_sols <- test_nsga2$parameters[idx, ]
nsga_pareto_front <-
  lapply(seq_along(idx), function(x)
    update_sol(i = x, sol_vector = nsga_sols[x, ]))
nsga_pareto_front <-
  gen_candidates(candidate_list = nsga_pareto_front)

saveRDS(
  list(
    `full_results` = test_nsga2,
    `pareto_front_plot` = plotParetoFront(inputData = nsga_pareto_front),
    `nsga_pareto_front` = nsga_pareto_front
  ),
  file = file.path(res_dir, 'NSGA_II_results.rds')
)
>>>>>>> e29de9b (Updates:)
