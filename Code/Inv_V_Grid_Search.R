source('MOSA Functions.R')
nServers <- 16
nVar <- 4
nRep <- 20
res_dir  <- file.path(".", "Data")

read_init <- T
use_test_bench <- T
inverted_V_logical <- T
jackson_envir <- new.env()
optim_type <- c('max', 'min','min')
starter_data <-
readRDS(file.path('Data', 'Medium Testing Initial Solution (4 queues).rds'))
queues_df <- starter_data$network_df
n_queues <- nVar <- queues_df[,.N]
sys.source('Jackson Network Test Bench.R',envir = jackson_envir)
obj_function_list <- 
    grep(pattern = 'TB_',
        x = lsf.str(),
        value = T)
init_sol <- c(1, rep(0, (nVar - 1)))
sim_length <- 2500
warmup <- 200

possible_allocs <- data.table(permute_with_duplicates(v = c(0,seq(nServers)),n = 4))[,sum := apply(.SD,1,sum)][sum == nServers,][,sum := NULL]
possible_allocs <- lapply(seq(nrow(possible_allocs)),function(i) list('Allocation' = unlist(possible_allocs[i,]))) 
possible_allocs <- lapply(X = seq_along(possible_allocs),
FUN = function(i){
    possible_allocs[[i]]$name = paste0('Allocation_',i)
    return(possible_allocs[[i]])
})

allocation_list <- 
unlist(x = lapply(
      X = possible_allocs,
      FUN = function(i) {
        rep(x = list(i$Allocation), times = nRep)
      }
    ),recursive = F)

replication_list <-
rep(x = seq(nRep),
    times = length(possible_allocs))

index_list  <- as.vector(sapply(
    seq_along(possible_allocs),
    FUN = function(i)
      rep(x = i, times = nRep)
  ))

test <- mclapply(
      X = seq_along(allocation_list),
      FUN = ocbaUpdate,
      mc.cores = availableCores(),
      arg_list = allocation_list,
      job_list = replication_list
    )

possible_allocs <- lapply(
    X = possible_allocs,
    FUN = function(Object) {
    obj_ind = which(possible_allocs %c% 'name' == Object$name)
    stats <- rbindlist(test[which(index_list== obj_ind)])
    Object <-
        updateSimStats(i = Object,
                        data = stats,
                        new_sol = T)
    }
) 

pareto_set <- updateParetoSet(pSet = list(),candidate_list = possible_allocs)
save.image(file = file.path(res_dir,'Grid Search Pareto Set (Medium Network).Rdata')) 
