source('.Rprofile')
source(file.path('Simulations', 'Minnesota MH Network Simulation.R'))
siteInfo <-
  data.table(readRDS(
    file.path('Simulations', 'Function Requirements', 'Rates5.rds')
  ))
initial_trials <- 20
args <- commandArgs(trailingOnly=TRUE)
if(length(args) > 0){
  obj_function_list <- list(as.character(args[1]))
} else {
  obj_function_list <- c("mh_distance_range")
}
warmup <- 30
sim_length <- 200
use_test_bench <- F
init_sol <- unlist(sapply(
  X = copy(siteInfo)[!duplicated(Bed_Group)][, .N, by = Facility_name][, N],
  FUN = function(i)
    c(1, rep(x = 0, times = i - 1))
))
uB <- rep(1,siteInfo[!duplicated(Bed_Group),.N])
lB <- rep(0,siteInfo[!duplicated(Bed_Group),.N])

optim_fun <- function(vec){
  vec <- decode(vec,.envir = .GlobalEnv)
  data <- CostFunction(sol = vec,logic = T,.envir = .GlobalEnv)
  data <- objective_Metrics(x = data,.envir = .GlobalEnv)
  return(apply(data[,replication := NULL],2,mean))
}

SA_results <- optim_sa(optim_fun,start = init_sol,maximization = F,trace = T,lower = lB,upper = uB)
saveRDS(SA_results,
        file = file.path(
          ".",
          'Data',
          'Full Sim Results',
          'Single_Obj_Results',
          paste(unlist(obj_function_list), 'results.rds', sep = '_')
        ))
