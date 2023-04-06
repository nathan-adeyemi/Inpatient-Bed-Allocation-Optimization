rm(list = ls())
source('.Rprofile')
bi_objective <- F
source(
  file.path(
    "~",
    "MH_Simulation",
    "Inpatient Bed Allocation Optimization",
    "Code",
    "Test_Bed_Opt_Setup.R"
  )
)

true_pareto_set <-
  readRDS(
    file.path(
      '~',
      'MH_Simulation',
      'Inpatient Bed Allocation Optimization',
      'Data',
      'Test Networks True Pareto Sets',
      paste0(size,' Network True Pareto Set.rds')
    )
  )

nTweak_vec <- c(3,7,12)
param_df <- rbind(
  data.table(sched_type = 'quadratic', t_damp = seq(0, 0.5, 0.025)[-1]),
  data.table(sched_type = 'linear', t_damp = seq(0, 1, 0.05)[-1]),
  data.table(sched_type = 'log_mult', t_damp = seq(2, 6, .5)),
  data.table(sched_type = 'exponential', t_damp = seq(0, 1, 0.05)[-1])
  )[rep(1:.N,each = length(nTweak_vec))
    ][, nTweak := rep(nTweak_vec, times = .N/length(nTweak_vec))]
res_dir <-
  file.path(".",
            "Data",
            "Sample MOSA Results",
            paste0(size, ' Hyperparamter Grid Search Results'))
if (!dir.exists(res_dir)) {
  dir.create(res_dir)
}

slurm_results_dir <- '~/_rslurm_hyperparameter_search/results_folder'

orig_dir <- getwd()
setwd('/home/adeyemi.n/')
sjob <-
  slurm_apply(
    f = DB_PSA,
    params = param_df,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = c('max','min','min'),
    nVar = queues_df[, .N],
    use_test_bench = use_test_bench,
    total_servers = total_servers,
    generate_plots = F,
    hyper = T,
    results_directory = slurm_results_dir,
    init_sol = init_sol,
    continue_previous = F,
    print_it_results = F,
    nodes = 20,
    processes_per_node = 1,
    submit = F,
    jobname = 'hyperparameter_search',
    global_objects = ls(),
    r_template = file.path(orig_dir,'Code','slurm_hyperparameter_template.txt'),
    sh_template = file.path(orig_dir,'Code','slurm_job_template.txt')
  )

setwd(orig_dir)
browser()

results_df <-
  rbindlist(lapply(
    X = file.path(slurm_results_dir,list.files(slurm_results_dir)),
    FUN = function(i){
      i <- readRDS(i)
      with(
        i,
        data.table(
          'sched_type' = sched_type,
          't_damp' = t_damp,
          'nTweak' = nTweak,
          'execution_time' = execution_time,
          'nIterations' = total_iterations,
          'nReplications' = nReplications,
          'pareto_set' = list(pSet)
        )
      )}
  ))

results_df <- results_df[, pareto_len := length(pareto_set[[1]]), by = list(sched_type, t_damp)
                         ][, percent_correct := pareto_perc_correct(pareto_set), by = list(sched_type, t_damp)
                           ][, non_true_pSet := pareto_perc_correct(pareto_set,extras = T), by = list(sched_type, t_damp)
                            ][order(-percent_correct,non_true_pSet, nIterations, nReplications, decreasing = F), ]

saveRDS(object = results_df,
        file = file.path(
          '.',
          'Data',
          paste(size, 'hyperparameter_search_results.rds', sep = '_')
        ))
