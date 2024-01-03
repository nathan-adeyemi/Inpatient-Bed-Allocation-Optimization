rm(list = ls())
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

nRep <- 20
res_dir <-
  file.path(".", "Data", 'Test Networks True Pareto Sets', size)
now <- Sys.time()
if (!file.exists(file.path(res_dir, paste(size, 'Network Simulation lists.rds')))) {
  if (!file.exists(file.path(res_dir, 'possible_allcoations.rds'))) {
    possible_allocs <- permute_with_duplicates(total_servers, n_queues)
   
    possible_allocs <- mclapply(
      X = seq(nrow(possible_allocs)),
      FUN = function(i)
        list("Allocation" = unlist(possible_allocs[i,])),
      mc.cores = availableCores()
    )
    
    possible_allocs <- mclapply(
      X = seq_along(possible_allocs),
      FUN = function(i) {
        possible_allocs[[i]]$name <- paste0("Allocation_", i)
        return(possible_allocs[[i]])
      },
      mc.cores = availableCores()
    )
    
    saveRDS(possible_allocs,
            file = file.path(res_dir, 'possible_allocations.rds'))
  } else {
    possible_allocs <-
      readRDS(file = file.path(res_dir, 'possible_allocations.rds'))
  }
  if(size == 'Large'){
  slurm_results_dir <- '~/_rslurm_hyperparameter_search/results_folder'
  inv.env <- environment()
  dir_0 <- getwd()
  sjob <- slurm_map(x = possible_allocations,
                    f = gridSearchSubFn,
                    cpus_per_node = 126,
                    nodes = 8,
                    jobname = 'grid_search',
                    r_template = file.path(dir_0,'Code','slurm_txt_templates','slurm_grid_R_template.txt'),
                    sh_template = file.path(dir_0,'Code','slurm_txt_templates','slurm_grid_job_template.txt'),
                    global_objects = ls(),
                    slurm_options = list(`partition`='long'),
                    submit = F)
  browser()
  
  possible_allocations <- mclapply(
    X = file.path(slurm_results_dir,list.files(slurm_results_dir)),
    readRDS,
    mc.cores = availableCores())
  } else {
    grid_search_results <-
      gridSearchSubFn(unique_allocations = possible_allocations, .envir = .GlobalEnv)
    saveRDS(grid_search_results,
            file = file.path(res_dir, paste0(size, " Allocations with Sim Stats.rds")))
  }

  cat("Simulations Complete")
} else {
  sim_list <- readRDS(file.path(res_dir, paste(size, 'Network Simulation lists.rds')))
  list2env(x = sim_list,envir = .GlobalEnv)
  
  # Sorting solutions into Pareto and other fronts (Bi-Objective)
  
  # test <- lapply(test,function(i){
  #   i[,ncol(i) := NULL]
  # })
  
  optim_type <- c('max','min')
  
  possible_allocs <- lapply(
    X = possible_allocs,
    FUN = function(Object) {
      obj_ind <- which(possible_allocs %c% "name" == Object$name)
      stats <- rbindlist(test[which(index_list == obj_ind)])
      Object <-
        updateSimStats(i = Object,
                       data = stats,
                       new_sol = T)
    }
  )
}
cat("Sorting into Front w/ 2 Objectives")

pareto_set <-
  updateParetoSet(pSet = list(),
                  candidate_list = possible_allocs)

jpeg(file = file.path(res_dir,paste0(size, " Network True Pareto Front.jpeg")))
plotParetoFront(
  inputData = pareto_set$pSet,
  plot_replications = F,
  plot_initial_point = F,
  plot_ideal_point = F
)
dev.off()

execution_time <- as.numeric(Sys.time() - now)
save(list = ls(all.names = T),file = file.path(res_dir,paste0(size, " True Pareto Set.rdata")))

