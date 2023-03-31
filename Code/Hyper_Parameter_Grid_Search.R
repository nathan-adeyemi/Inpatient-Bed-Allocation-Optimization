rm(list = ls())
source('.Rprofile')
source(file.path("~","MH_Simulation","Inpatient Bed Allocation Optimization","Code","Test_Bed_Opt_Setup.R"))

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
slurm_func <- function(sched_type,t_damp,nTweak) {
  DB_PSA(
    continue_previous = F,
    print_it_results = F,
    sched_type = sched_type,
    t_damp = t_damp,
    nTweak = nTweak,
    sim_length = sim_length,
    warmup = warmup,
    obj_function_list = obj_function_list,
    optim_type = optim_type,
    nVar = queues_df[, .N],
    use_test_bench = use_test_bench,
    total_servers = 4 * queues_df[, .N],
    generate_plots = F,
    hyper = T
  )
}
test_param_df <- param_df[seq(4),]
orig_dir <- getwd()
setwd('/home/adeyemi.n/')
sjob <-
  slurm_apply(
    f = slurm_func,
    params = test_param_df,
    nodes = 2,
    processes_per_node = 1,
    submit = F,
    jobname = paste(size,'hyperparameter_search',sep = '_'),
    global_objects = ls(),
    r_template = file.path(orig_dir,'Code','slurm_hyperparameter_template.txt'),
    sh_template = file.path(orig_dir,'Code','slurm_job_template.txt')
  )
browser()
results <- get_slurm_out(sjob,outtype = 'raw',wait = T)
saveRDS(results,file = file.path(orig_dir,'Data',paste(size,'hyperparameter_search_results.rds',sep = '_')))
setwd(orig_dir)
# cleanup_files(sjob)
param_df[row, `:=`(
  nIterations = res$total_iterations,
  nReplications = res$nReplications,
  pareto_set = list(res$pSet)
)]
param_df[, pareto_len := length(pareto_set[[1]]), by = list(sched_type, alpha)
         ][, percent_correct := pareto_perc_correct(pareto_set), by = list(sched_type, alpha)
           ][order(-percent_correct, nIterations, nReplications, decreasing = F), ]
save.image(file.path(res_dir, 'Completed Search environment.rdata'))
