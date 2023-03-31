rm(list = ls())
source('.Rprofile')
source(file.path("~","MH_Simulation","Inpatient Bed Allocation Optimization","Code","Test_Bed_Opt_Setup.R"))

nRep <- 20
res_dir <- file.path(".", "Data", 'Test Networks True Pareto Sets')

possible_allocs <-
  data.table(permute_with_duplicates(v = c(0, seq(nServers)), n = nVar))[, sum := apply(.SD, 1, sum)][sum == nServers,][, sum := NULL]
possible_allocs <- lapply(
  X = seq(nrow(possible_allocs)),
  FUN = function(i)
    list("Allocation" = unlist(possible_allocs[i, ]))
)

possible_allocs <- lapply(
  X = seq_along(possible_allocs),
  FUN = function(i) {
    possible_allocs[[i]]$name <- paste0("Allocation_", i)
    return(possible_allocs[[i]])
  }
)

allocation_list <-
  unlist(x = lapply(
    X = possible_allocs,
    FUN = function(i) {
      rep(x = list(i$Allocation), times = nRep)
    }
  ),
  recursive = F)

replication_list <-
  rep(x = seq(nRep),
      times = length(possible_allocs))

index_list <- as.vector(sapply(
  seq_along(possible_allocs),
  FUN = function(i) {
    rep(x = i, times = nRep)
  }
))

test <- mclapply(
  X = seq_along(allocation_list),
  FUN = ocbaUpdate,
  mc.cores = availableCores(),
  arg_list = allocation_list,
  job_list = replication_list,
  envir = .GlobalEnv
)

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

pareto_set <-
  updateParetoSet(pSet = list(),
                  candidate_list = possible_allocs,
                  envir = .GlobalEnv)
save.image(file = file.path(res_dir, paste0(size, " Network True Pareto Set.Rdata")))
