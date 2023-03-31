ocba <- 
  function(candidate_list,.envir = parent.frame()) {
    
    # Multi-Objective Optimal Computing Budget Allocation 
    N_Total <- 20 * length(candidate_list)
    N_Total <<- N_Total
    psiTarget <- 5.6e-6 #Value from MOCBA Paper (Lee et. al. 2004)
    # psiTarget <- -1
    delta <- .envir$delta
    first_time = TRUE
    N_replicated <- sum(candidate_list %c% 'Replications') # - (candidate_list[length(candidate_list)] %c% 'Replications')
    K <- ceil(.envir$nTweak / 3)
    K <-
      sl_ifelse(
        test = length(candidate_list) < K,
        yes = length(candidate_list),
        no = K
      ) 
    
    candidate_list <-
      lapply(X = candidate_list,
             FUN = procedureI_func,
             all_candidates = candidate_list,
             .envir = .envir)
    
    # Procedure I Step 2
    while ((N_Total  - N_replicated) > 0 & all(candidate_list %c% 'Psi' > psiTarget)){ 
      # Loop breaks once there are 0 replications left to allocate or one of the solutions is below target psi value  
      
      # Procedure 1 Step 1
      psiOrder <- order(candidate_list %c% 'Psi')
      sP_indices <- psiOrder[seq(K)]
      sP <- candidate_list[sP_indices] 
      
      delta <- min(delta,(N_Total  - N_replicated))
      # Procedure I Step 3  
      # Procedure II Step 1
      for (candidate_ind in seq_along(sP)) {
        sP[[candidate_ind]] <-
          procedureII_func(Object = sP[[candidate_ind]],
                           kVal = K,
                           all_candidates = candidate_list,
                           include_All_D = T,
                           .envir = .envir)
      }
      # Procedure II Step 2
      sP = lapply(sP,function(d){
        # d$deltaPsiD <- ifelse(test = length(d$psiID) > 1,
        #                       yes = sum(d$psiID[sP_indices]),
        #                       no = abs(min(0, d$psiID))) # Do we include all delta psiD or just the delta psi_i
        d$deltaPsiD <- sum(abs(d$psiID[sP_indices]))
        d
      })
      
      sP <- sP[order(sP %c% 'deltaPsiD',decreasing = T)]
      
      # Procedure II step 3
      k_test <- min(K,ifelse(test = any((sP %c% 'deltaPsiD') == 0),
                             yes = which(sP %c% 'deltaPsiD' == 0) - 1,
                             no = K)) # Minimum of K and the Index of last nonzero deltaPsi
      psiRef <- sP[[k_test]]$deltaPsiD
      deltaWP <- (psiRef * delta)/sum(abs((sP %c% 'deltaPsiD')[seq(k_test)]))
      # Assigning additional replications to each of the solutions
      addReps <-
        smart_round({function(i) delta * (i/sum(i))}(sapply(
          X = seq_along(sP),
          FUN = addReps_fun,
          candidate_list = sP,
          k_test_val = k_test,
          deltaRef = deltaWP,
          reference_Psi = psiRef,
          .envir = .envir)))
      
      if(sum(addReps) == 0){
        break
      }
      
      for (i in seq_along(sP)) {
        sP[[i]]$addReps = addReps[i]
      }
      
      N_replicated %+% sum(addReps)
      
      ### Assign Additional Replications to most Promising Allocations
      allocations <- list()
      jobs <- c()
      job_names <-  c()
      
      for (i in seq_along(sP)){
        if(sP[[i]]$addReps > 0){
          job_names <- append(job_names,rep(sP[[i]]$name,each = sP[[i]]$addReps))
          allocations <- append(allocations,rep(list(sP[[i]]$Allocation),each = sP[[i]]$addReps))
          jobs <- append(jobs,seq(sP[[i]]$addReps) + max(sP[[i]]$Cost$replication))
          sP[[i]]$Replications %+% sP[[i]]$addReps
        }
      }
      
      alloc_list <-  seq(sum(addReps))
      
      # Perform Additional Replications 
      if (Sys.info()['sysname'] == 'Windows'){
        cl <- makePSOCKcluster(detectCores(logical = TRUE) - 1)
        registerDoParallel(cl)
        newResults <- foreach(test_allocation = alloc_list, 
                              .export = c(ls(envir = .GlobalEnv)[grepl('',ls(envir = .GlobalEnv))],'i','prefix','siteInfo','best','it','temp', 'maxChange',
                                          'age.frequency','ocbaUpdate','CostFunction','barrierRates','distance.matrix',
                                          'time.matrix','edArrival_rate','MH.Network.sim','seed_val','full_sim','allocations','jobs'),
                              .packages = c('simmer','gtools','dplyr','compiler','boot','parallel','simpleboot',
                                            'tidyverse','readxl','magrittr','data.table','EnvStats')) %dopar% ocbaUpdate(arg = test_allocation)
        stopCluster(cl)
        reap_zombies()
      } else {
        newResults <-
          mclapply(
            X = alloc_list,
            FUN = ocbaUpdate,
            mc.cores = availableCores(),
            arg_list = allocations,
            job_list = jobs,
            .envir = .envir
          )
      }
      sP <- mclapply(
        X = sP,
        FUN = function(Object, res) {
          new_stats <- rbindlist(res[which(job_names == Object$name)])
          Object <-
            updateSimStats(i = Object,
                           data = new_stats,
                           new_sol = F)
          return(Object)
        },
        res = newResults,
        mc.cores = availableCores()
      )
      candidate_list[psiOrder[seq(K)]] <- sP
      
      candidate_list <-
        lapply(X = candidate_list,
               FUN = procedureI_func,
               all_candidates = candidate_list,
               .envir = .envir)
      extraReps <<- T
    }
    itReps <<- N_replicated
    
    return(list(sP = candidate_list[order(candidate_list %c% 'Psi')
                                    ][seq(K)],
                non_sP = candidate_list[order(candidate_list %c% 'Psi')
                                        ][(K+1:length(candidate_list))]))
  } 