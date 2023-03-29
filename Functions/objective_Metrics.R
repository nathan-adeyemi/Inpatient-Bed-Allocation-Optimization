<<<<<<< HEAD
objective_Metrics <- function(x, .envir = parent.frame()){
  
  # Applies the selected objective functions to the simulation data
  
  # Inputs: 
  #   x: (Lisr) A list of Simmer output data.frames
  
  # Returns:
  #   data: (Data.table) A data.table of the calculated objective metrics for each simulation replication 
  
  data <- lapply(
    X = .envir$obj_function_list,
    FUN = function(func) {
      do.call(what = func,
              args = list(x))
    }
  )
    data <- Reduce(merge,data)
    return(data)
}
=======
objective_Metrics <- function(data,fun_list = NA, envir = parent.frame()){
  data %>% 
    {function(data) lapply(fun_list,FUN = function(i){ 
      eval(parse(text = paste0(i,'(data)')),envir = .GlobalEnv)
      })}() %>%
    Reduce(merge,.) %>% data.table()
}

simple_mean_comparison <- function(metric, df, strict = T, envir = parent.frame()) {
  if (strict)
    df[soln_num == 2, sapply(.SD, mean), .SDcols = metric] < df[soln_num == 1, sapply(.SD, mean), .SDcols = metric]
  else
    df[soln_num == 2, sapply(.SD, mean), .SDcols = metric] <= df[soln_num == 1, sapply(.SD, mean), .SDcols = metric]
}

soln_comparison <-
  function(s1,
           s2,
           stat = get('stat_logical', envir = .GlobalEnv),
           alpha = 0.05, 
           envir = parent.frame()) {
    comp_df <- rbind(copy(s1$Cost)[,soln_num := 1],
                     copy(s2$Cost)[,soln_num := 2])
    
    alpha <- 0.05
    #Less strict alpha value if there are few simulation replications
    if(any(sapply(list(s1,s2),function(i) any(i$Obj_var > i$Obj_mean)))){ 
      alpha <- 0.1
    } 
    
    # Return T/F if the tested difference in quantities is negative and statistically significant
    # T = s2 dominates s1
    
    indx <-
      setdiff(colnames(comp_df), c('soln_num', 'replication'))
    for (col in indx) {
      # if the column is to be maximized, multiply values by -1
      comp_df[[col]] <-
        comp_df[[col]] * sapply(
          X = optim_type,
          FUN = switch,
          'min' = 1,
          'max' = -1
        )[match(col, indx)]
    }
    if (stat) {
      criteria_1 <- sapply(
        X = seq_along(indx),
        FUN = function(variable) #test: s1 - s2 = 0 i.e. s2 == s1 
          t.test(
            formula = formula(paste0(indx[variable], ' ~ soln_num')),
            data = comp_df
          )$p.value
      ) > alpha
      
      criteria_2 <- sapply( #s1 - s2 > 0  i.e. s2 < s1
        X = seq_along(indx),
        FUN = function(variable)
          t.test(
            formula = formula(paste0(indx[variable], ' ~ soln_num')),
            data = comp_df,
            alternative = 'g'
          )$p.value
      ) < alpha
      # Return T if the tested allocation's quantity is less (no statistical tests)
    } else {
      criteria_1 <-
        all(sapply(
          X = seq_along(indx),
          FUN = simple_mean_comparison,
          df = comp_df,
          strict = F
        ))
      criteria_2 <-
        any(sapply(
          X = seq_along(indx),
          FUN = simple_mean_comparison,
          df = comp_df,
          strict = T
        ))
    }
    return(all(mapply(sum,criteria_1,criteria_2)))
  }

p_accept <- function(curr_temp,initial_temp = temp_init) 1 - (exp(initial_temp - curr_temp) / exp(initial_temp))

noisyNonDominatedSorting <- function (inputData, envir = parent.frame()) 
{ 
  #Modified from the fastNonDominatedSorting function in the nsga2R v1.1 package
  popSize = length(inputData)
  idxDominators = vector("list", popSize)
  idxDominatees = vector("list", popSize)
  for (i in 1:(popSize - 1)) {
    for (j in i:popSize) {
      if (i != j) {
        xi = inputData[[i]]
        xj = inputData[[j]]

        # Test if xi dominates xj
        crit_1 <- soln_comparison(s1 = xj,s2 = xi,stat = T)

        # Test if xj dominates xi
        crit_2 <- soln_comparison(s1 = xi,s2 = xj,stat = T)
        # browser(expr = any(list(xi,xj) %c% 'name' == 'Tourney_114_Candidate_5'))

        # Assign dominating and dominated solutions
        if (crit_1 && !crit_2) {
          idxDominators[[j]] = c(idxDominators[[j]], 
                                 i)
          idxDominatees[[i]] = c(idxDominatees[[i]], 
                                 j)
        } else if (crit_2 && !crit_1) {
          idxDominators[[i]] = c(idxDominators[[i]], 
                                 j)
          idxDominatees[[j]] = c(idxDominatees[[j]], 
                                 i)
        }
      }
    }
  }
  
  noDominators <- lapply(idxDominators, length)
  rnkList <- list()
  rnkList <- c(rnkList, list(which(noDominators == 0)))
  solAssigned <- c()
  solAssigned <- c(solAssigned, length(which(noDominators == 
                                               0)))
  while (sum(solAssigned) < popSize) {
    Q <- c()
    noSolInCurrFrnt <- solAssigned[length(solAssigned)]
    if(noSolInCurrFrnt > 0){
    for (i in 1:noSolInCurrFrnt) {
      
      solIdx <- rnkList[[length(rnkList)]][i]
      hisDominatees <- idxDominatees[[solIdx]]
      for (i in hisDominatees) {
        noDominators[[i]] <- noDominators[[i]] - 1
        if (noDominators[[i]] == 0) {
          Q <- c(Q, i)
        }
      }
    }
    rnkList <- c(rnkList, list(sort(Q)))
    solAssigned <- c(solAssigned, length(Q))
    }
  }
  rnkVec <- rnkList
  names(rnkVec) <- seq_along(rnkVec)
  rnkVec <- as.numeric(data.table(stack(rnkVec))[order(values),ind])
  return(list('rnkList' = rnkList,
              'rnkVec' = rnkVec))
}

updateParetoSet <- function(pSet,candidate_list, envir = parent.frame()){
  n_obj <- length(optim_type)
  pSet <-  append(pSet,candidate_list)
  pSet <- removeDuplicateSolutions(pSet)
  pSetCopy <- pSet
  if(length(pSet) > 1){
    ranks <-
      noisyNonDominatedSorting(inputData = pSet)
    # pSet <- lapply(pSet, procedureI_func, all_candidates = pSet)
  } else {
    ranks <- list(ranks = 1, rnkList = list(1))
  }
  pSet <- pSet[ranks$rnkList[[1]]]
  return(list('pSet' = pSet, 'ranks' = ranks))
}

findBestbyDistance <- function(pSet, rankInfo, envir = parent.frame()) {
  if (length(pSet) > 1) {
    obj_means <- apply(
      X = as.matrix(t(pSet %c% 'Obj_mean')),
      MARGIN = 2,
      FUN = function(u)
        matrix(unlist(u))
    )
    
    g_ideal_dist <- sapply(
      X = seq(ncol(obj_means)),
      FUN = function(index)
        eval(parse(
          text = paste0('which.', optim_type[index], '(obj_means[,', index, '])')
        ))
    )
    g_ideal_dist <-
      sapply(
        X = seq_along(g_ideal_dist),
        FUN = function(i)
          as.matrix(pSet[[g_ideal_dist[i]]]$Cost)[, i + 1]
      )
    
    if (is.list(g_ideal_dist)) {
      multiple <- Reduce(Lcm, sapply(g_ideal_dist, length))
      g_ideal_dist <-
        sapply(g_ideal_dist, function(i)
          unlist(rep(i, multiple / length(i))))
    }
    g_ideal_CI <<-
      apply(
        X = g_ideal_dist,
        MARGIN = 2,
        FUN = function(i)
          ci_as_text(t.test(i, conf.level = 0.95)$conf.int)
      )
    
    # divergences <- sapply(
    #   X = pSet,
    #   FUN = function(i) {
    #     sd_cols <- setdiff(colnames(i$Cost), 'replication')
    #     return(kldiv(
    #       mu1 = apply(g_ideal_dist, 2, mean),
    #       mu2 = i$Cost[, sapply(.SD, mean), .SDcols = sd_cols],
    #       sigma1 = cov(g_ideal_dist),
    #       sigma2 = cov(i$Cost[, -1]),
    #       symmetrized = T
    #     ))
    #   }
    # )
    divergences <- sapply(
      X = pSet,
      FUN = function(i) {
        sd_cols <- setdiff(colnames(i$Cost), 'replication')
        return(bhattacharyya.dist(
          mu1 = apply(g_ideal_dist, 2, mean),
          mu2 = i$Cost[, sapply(.SD, mean), .SDcols = sd_cols],
          Sigma1 = cov(g_ideal_dist),
          Sigma2 = cov(i$Cost[, -1])
        ))
      }
    )

      selection_probs <- mod_softmax(divergences)
      pareto_set <<- pSet <- lapply(X = seq_along(pSet),FUN = function(i){
        pSet[[i]]$Divergence <- divergences[i]
        pSet[[i]]$P_Selection <- round(selection_probs[i],digits = 4)
        return(pSet[[i]])
      })
      
      best <-
        sample(pSet,
               size = 1,
               prob = selection_probs)

    }else {
    g_ideal_CI <<- unlist(best$Obj_CI)
    best <- pSet[[1]]
    }
  return(best)
}

findBestbyDominanceMatrix <-
  function(current_best, candidate_list, stat_comp = F, envir = parent.frame()) {
    
    # Creates dominance matrices
    # Key:
    # N_Objectives = row solution dominates column solution
    # (1:N_objectives-1) = row solution and column solution are pareto equivalent
    # 0 = row solution is dominated by column solution
    
    dominance_matrix <- dominance_matrix_non_stat <-
      matrix(
        data = NA,
        nrow = length(candidate_list),
        ncol = length(candidate_list)
      )
    
    for (i in seq_along(candidate_list)) {
      for (j in seq_along(candidate_list)) {
        if (i != j) {
          dominance_matrix[i, j] <-
            sum(!soln_comparison(
              s1 = candidate_list[[i]],
              s2 = candidate_list[[j]],
              stat = stat_comp
            ))
        }
      }
    }
    
    if (length(candidate_list) >= 2) {
      idx <- which.max(rowSums(dominance_matrix, na.rm = T) /
                         (ncol(dominance_matrix) - 1))
    } else {
      idx = 1
    }
    
    if (length(idx) > 1) {
      idx <- idx[which.min(candidate_list[idx] %c% 'Psi')]
    }
    
    if (!identical(candidate_list[[idx]], best)) {
      Prev_Best <- current_best
      best <- candidate_list[[idx]]
    } else {
      Prev_Best <- current_best
      best <- current_best
    }
    return(best)
  }

findBestbySequentialComparison <- function(candidate_list,
                                           stat_comp = get('stat_logical'), envir = parent.frame()) {
  for (i in candidate_list) {
    # Objective Functions Statistical Tests (Non - parametric test)
    test <-  soln_comparison(best, i, stat = stat_comp)
    
    psi_diff <-
      (list(best, i) %c% 'Psi') %>% {
        function(i)
          max(i) - min(i)
      }()
    grad_change <-
      (list(best, i) %c% 'Obj_mean') %>% {
        function(i)
          (i[, 2] - i[, 1]) / i[, 1]
      }()
    psiList <- append(psiList, psi_diff)
    test_val <- runif(1)
    comp_val <- p_accept(sum(!test), psi_diff, temp, T)
    if (test_val < comp_val) {
      # Add previous best to archive
      Prev_Best <- append(Prev_Best, list(best))
      # Assign the surviving solution as the newest "best" solution
      best <- i
    } else {
      Rejected <- append(Rejected, list(i))
    }
  }
  return(best)
}

findBestbyPsi <- function(candidate_list, envir = parent.frame()){
  candidate_list <- lapply(candidate_list,procedureI_func,all_candidates = candidate_list)
}

updateHistory <- function(pSet, candidate_list, history, envir = parent.frame()) {
  if (length(history) != 0) {
    history <- append(history,
                      list(list(
                        Rejects = setdiff(candidate_list, pareto_set),
                        itBest = pareto_set
                      )))
  } else{
    history <- list(list(
      Rejects = setdiff(candidate_list, pareto_set),
      itBest = pareto_set
    ))
  }
  
}

removeDuplicateSolutions <- function(front, envir = parent.frame()){
  return(front[!duplicated(lapply(
    X = front,
    FUN = function(i) i$name))])
}

compareIterationFronts <- function(curr_front,prev_front, envir = parent.frame()){
  return(identical(
    removeDuplicateSolutions(curr_front) %c% 'name',
    removeDuplicateSolutions(prev_front)  %c% 'name'
  ))
}

cool_temp  <-
  function(initial_temperature,
           alpha,
           current_iteration,
           exponential = F,
           linear = F,
           log_mult = F,
           quad_cool = F,
           constant = F, envir = parent.frame()) {
    if (linear) {
      temp <-  initial_temperature / (1 + (alpha * current_iteration))
    } else if (log_mult) {
      temp <-  initial_temperature / (1 + alpha * (log(1 + current_iteration)))
    } else if (quad_cool) {
      temp <- initial_temperature / (1 + (alpha * current_iteration ^ 2))
    } else if(exponential){
      alpha <- sl_ifelse(test = alpha > 0,
                         yes = alpha * -1,
                         no = alpha)
      temp <-  initial_temperature * current_iteration ^ alpha
    }
    return(temp)
  }

# Functions for Optimal Computing Budget Allocation -----------------------
addReps_fun <-
  function(p,
           candidate_list,
           k_test_val,
           deltaRef,
           reference_Psi) {
    if (p == k_test_val){
      deltaRef
    } else if (p < k_test_val){
      candidate_list[[p]]$deltaPsiD/reference_Psi * deltaRef
    } else {
      0
    }
  }

ocbaUpdate <-
  function(arg,
           arg_list,
           job_list,
           test_bench = get('use_test_bench', envir = .GlobalEnv)) {
    
    if(test_bench){
      objective_function_list <-  grep('TB_',lsf.str(envir = .GlobalEnv),value = T)
    } else {
      objective_function_list <-  grep('mh_',lsf.str(envir = .GlobalEnv),value = T)
    }
    
    temp_results <- CostFunction(sol = arg_list[[arg]],logic = FALSE,reps = 1) 
    temp_results <- data.table(objective_Metrics(data = temp_results,fun_list = objective_function_list))[,replication := job_list[arg]]
    return(temp_results)
  }

psiPval_subfunction <-  function(i,means,deviations) {
  z <- pnorm(
    q = as.numeric(means[, ..i]),
    mean = 0,
    sd = as.numeric(deviations[, ..i])
  )
  if (get(x = 'optim_type', envir = .GlobalEnv)[which(colnames(means) == i)] == 'max') {
    z <- 1 - z
  }
  return(z)
}

psiPVal <-
  function(main,
           indexed,
           hat = FALSE,
           self_hat = FALSE) {
    #P(F_jindex <= F_iindex)
    delta <- get('delta', envir = .GlobalEnv)
    x_bar = main$Obj_mean - indexed$Obj_mean
    if (self_hat) {
      sigma_est_body =  sqrt(
        main$Obj_var / (main$Replications + delta) + indexed$Obj_var / indexed$Replications
      )
    } else if (!hat) {
      sigma_est_body = sqrt(main$Obj_var / main$Replications + indexed$Obj_var /
                              indexed$Replications)
    } else{
      sigma_est_body = sqrt(
        indexed$Obj_var / (indexed$Replications + delta) + main$Obj_var / main$Replications
      )
    }
    p_indexed_better_main <- prod(
      sapply(
        X = colnames(x_bar),
        FUN = psiPval_subfunction,
        means = x_bar,
        deviations = sigma_est_body
      )
    )
    return(p_indexed_better_main)
  }

procedureI_func <- function(Object,all_candidates) {  # OCBA Procedure I function
  Object$Psi <- unname(sum(sapply(all_candidates,
                                  function(i)
                                    ifelse(
                                      test = !identical(i, Object),
                                      yes = psiPVal(main = Object, indexed = i),
                                      no = 0
                                    ))))
  
  return(Object)
} 

procedureII_func <- 
  function(Object,
           kVal,
           all_candidates,
           include_All_D = T) {
    # OCBA Procedure II Function
    # Arg include_All_D: True -> consider change in Psi for all other solutions 
    #                    False -> consider change in Psi for just solution i
    sub_val_1 = sub_val_2 = 1
    psiID <- c()
    num_obj = ncol(Object$Obj_mean)
    if (include_All_D) {
      Object$psiID <- round(x = sapply(
        X = all_candidates,
        FUN = function(d) {
          if (!identical(Object$name, d$name)) {
            ret <- unname(psiPVal(Object, indexed = d) - psiPVal(Object, indexed = d, hat = TRUE))
          } else {
            # equation (7)
            ret <- sum(sapply(all_candidates, function(j)
              psiPVal(Object, indexed = j))) - sum(sapply(all_candidates, function(j)
                psiPVal(Object, indexed = j, self_hat = T)))
          }
          return(ret)
        }),
        digits = 7)
      
    } else {
      ind <- which(Object$name == (all_candidates %c% 'name'))
      Object$psiID <-
        round(x = sum(sapply(all_candidates, function(j) {
          psiPVal(Object, indexed = j)
        })) -
          sum(sapply(all_candidates, function(j) {
            psiPVal(Object, indexed = j, self_hat = T)
          })), digits = 7)
    }
    return(Object)
  }

ocba <- 
  function(candidate_list,
           test_bench = get(x = 'use_test_bench',
                            envir = .GlobalEnv),envir = parent.frame()) {
    
    # Multi-Objective Optimal Computing Budget Allocation 
    N_Total <- 20 * length(candidate_list)
    N_Total <<- N_Total
    psiTarget <- 5.6e-6 #Value from MOCBA Paper (Lee et. al. 2004)
    # psiTarget <- -1
    delta <- get('delta',envir = .GlobalEnv)
    first_time = TRUE
    N_replicated <- sum(candidate_list %c% 'Replications') # - (candidate_list[length(candidate_list)] %c% 'Replications')
    K <- ceil(nTweak / 3)
    K <-
      sl_ifelse(
        test = length(candidate_list) < K,
        yes = length(candidate_list),
        no = K
      ) 
    
    candidate_list <-
      lapply(X = candidate_list,
             FUN = procedureI_func,
             all_candidates = candidate_list)
    
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
                           include_All_D = T)
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
        smart.round({function(i) delta * (i/sum(i))}(sapply(
          X = seq_along(sP),
          FUN = addReps_fun,
          candidate_list = sP,
          k_test_val = k_test,
          deltaRef = deltaWP,
          reference_Psi = psiRef)))
      
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
            job_list = jobs
          )
      }
      sP <- lapply(
        X = sP,
        FUN = function(Object, res) {
          new_stats <- rbindlist(res[which(job_names == Object$name)])
          Object <-
            updateSimStats(i = Object,
                           data = new_stats,
                           new_sol = F)
          return(Object)
        },
        res = newResults
        # mc.cores = availableCores()
      )
      candidate_list[psiOrder[seq(K)]] <- sP
      
      candidate_list <-
        lapply(X = candidate_list,
               FUN = procedureI_func,
               all_candidates = candidate_list)
      extraReps <<- T
    }
    itReps <<- N_replicated
    
    return(list(sP = candidate_list[order(candidate_list %c% 'Psi')
                                    ][seq(K)],
                non_sP = candidate_list[order(candidate_list %c% 'Psi')
                                        ][(K+1:length(candidate_list))]))
  } 

# Function for implementing the NSGA-II Algorithm -------------------------
objective_Metrics_nsga2 <- function(data,fun_list = NA,envir = parent.frame()){
  data %>% 
    {function(data) lapply(fun_list,FUN = function(i) eval(parse(text = paste0(i,'(data)'))))}() %>%
    Reduce(merge,.) %>% data.table()
}

sim_nsga2 <- function(inp_vec,envir = parent.frame()) {
  if (!all(inp_vec %% 1 == 0)) {
    inp_vec <- decode(inp_vec)
  }
  res <-
    run_test_bench(
      rep_nums = seq(10),
      network_df = queues_df[, server_count := inp_vec],
      multicore = F,
      sim_length = get(x = 'sim_length',envir = .GlobalEnv),
      warmup = get(x = 'warmup',envir = .GlobalEnv),
      inverted_V = get(x = 'inverted_V_logical',envir = .GlobalEnv)
    )
  return(apply(objective_Metrics_nsga2(
    res, fun_list = grep('TB_', lsf.str(envir = .GlobalEnv), value = T)
  )[, -1], 2, mean))
}

analytical_nsga2 <- function(inp_vec,envir = parent.frame()){
  if(!all(inp_vec %% 1 == 0)){
    inp_vec <- decode(inp_vec)
  }
  tryCatch({
    res <-
      calc_steady_state_metrics(queues_df[, server_count := inp_vec])
    return(res * c(-1, 1, 1))
  },
  error = function(e) {
    # err <- cat("ERROR :", conditionMessage(e))
    return (c(
      N_j_analytical = Inf,
      W_q_analytical = Inf,
      rho_analytical = Inf
    ))
  })
}

mcnsga2 <- function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
          upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
          generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
          MuDistIdx = 10,envir = parent.frame()) 
{
  # Modified version of the nsga2 function in the nsga2R package version 1.1
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  # cat("initializing the population")
  # cat("\n")
  parent <- t(sapply(1:popSize, function(u) array(runif(length(lowerBounds), 
                                                        lowerBounds, upperBounds))))
  parent <-
    cbind(parent, t(matrix(unlist(mclapply(
      X = split(parent, seq(nrow(parent))),
      FUN = fn,
      mc.cores = availableCores()
    )),ncol = popSize)))
  parent[,(varNo + 1)] <- -1 * parent[,(varNo + 1)]
  
  # cat("ranking the initial population")
  # cat("\n")
  ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + 
                                                             objDim)])
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  # cat("crowding distance calculation")
  # cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    cat("---------------generation---------------", iter, "starts")
    cat("\n")
    # cat("tournament selection")
    # cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    # cat("crossover operator")
    # cat("\n")
    childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    # cat("mutation operator")
    # cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    # cat("evaluate the objective fns of childAfterM")
    # cat("\n")
    childAfterM <- cbind(childAfterM,
                         t(matrix(unlist(
                           mclapply(
                             X = split(childAfterM, seq(nrow(childAfterM))),
                             FUN = fn,
                             mc.cores = availableCores()
                           )
                         ), ncol = popSize)))
    childAfterM[,(varNo + 1)] <- -1 * childAfterM[,(varNo + 1)]
    # cat("Rt = Pt + Qt")
    # cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    # cat("ranking again")
    # cat("\n")
    ranking <- fastNonDominatedSorting(parentNext[, (varNo + 
                                                       1):(varNo + objDim)])
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    # cat("crowded comparison again")
    # cat("\n")
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
    ]
    # cat("environmental selection")
    # cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    # cat("---------------generation---------------", iter, "ends")
    # cat("\n")
    if (iter != generations) {
      # cat("\n")
      # cat("********** new iteration *********")
      # cat("\n")
    }
    else {
      # cat("********** stop the evolution *********")
      # cat("\n")
    }
  }
  parent[,(varNo + 1)] <- -1 * parent[,(varNo + 1)]
  result = list(
    functions = fn,
    parameterDim = varNo,
    objectiveDim = objDim,
    lowerBounds = lowerBounds,
    upperBounds = upperBounds,
    popSize = popSize,
    tournamentSize = tourSize,
    generations = generations,
    XoverProb = cprob,
    XoverDistIndex = XoverDistIdx,
    mutationProb = mprob,
    mutationDistIndex = MuDistIdx,
    parameters = parent[,
                        1:varNo],
    objectives = parent[, (varNo + 1):(varNo +
                                         objDim)],
    paretoFrontRank = parent[, varNo + objDim +
                               1],
    crowdingDistance = parent[, varNo + objDim +
                                2])
  class(result) = "nsga2R"
  return(result)
}

# Function for Plotting MOSA Results --------------------------------------
normalize <- function(base,df,envir = parent.frame()){
  setDT(df)
  names <- unique(df$name)
  for (n in names){
    home <- base[n] %>% unlist()
    if (!exists('new_df')){
      new_df <- df[name == n] %>% mutate(value = as.numeric(value), 
                                         home = base[n] %>% unlist()) %>% 
        mutate(value = (value - home)/home * 100) %>% dplyr::select(!home)
    } else {
      new_df <- rbind(new_df, df[name == n] %>% mutate(value = as.numeric(value), 
                                                       home = base[n] %>% unlist()) %>% 
                        mutate(value = (value - home)/home * 100) %>% dplyr::select(!home))
    }
  }
  return(new_df)
}

plotParetoFront <- function(inputData,plot_angle = 120,envir = parent.frame()){
  if (any(class(inputData) == 'data.table')) {
    inputData <- as.matrix(inputData)
    paretoPlot <- scatterplot3d(inputData, grid = F)
    addgrids3d(inputData, grid = c("xy", "xz", "yz"))
    paretoPlot$points3d(inputData, pch = 16, type = 'h')
  } else if (class(inputData) == 'list') {
    inputData2 <-
      rbindlist(lapply(
        inputData,
        FUN = function(i)
          i$Cost[, -1][, sol := i$name]
      ))
    n_sols <- length(unique(inputData2$sol))
    colors <-
      palette(value = hcl.colors(n = n_sols, 
                                 palette = 'Dynamic'))[sample(seq(n_sols),n_sols,replace = F)]
    color_inputData <-
      colors[as.numeric(as.factor(inputData %c% 'name'))]
    inputData <-
      candidateSettoMatrix(set = inputData, attr = 'Obj_mean')
    color_inputData2 <-
      colors[as.numeric(as.factor(inputData2[, sol]))]
    data_groups <- inputData2[, sol]
    axis_labels <-
      str_to_title(gsub(
        x = colnames(inputData2)[1:3],
        pattern = '_',
        replacement = ' '
      ))
    paretoPlot <-
      scatterplot3d(
        copy(inputData2)[, sol := NULL],
        grid = F,
        pch = 8,
        color = alpha(color_inputData2,.5),
        angle = plot_angle,
        main = 'Pareto Set Objective Metrics',
        xlab = axis_labels[1],
        ylab = axis_labels[2],
        zlab = axis_labels[3]
      )
    addgrids3d(inputData2, grid = c("xy", "xz", "yz"),angle = plot_angle)
    paretoPlot$points3d(inputData,
                        pch = 16,
                        type = 'h',
                        col = color_inputData,
                        cex = 1.25)
  }
}

# Adds grids for scatterplot3d plots
addgrids3d <- function(x, y=NULL, z=NULL, grid = TRUE,
                       col.grid = "grey", lty.grid = par("lty"),
                       lab = par("lab"), lab.z = mean(lab[1:2]),
                       scale.y = 1, angle = 40,
                       xlim=NULL, ylim=NULL, zlim=NULL,envir = parent.frame()){
  
  
  if(inherits(x, c("matrix", "data.frame"))){
    x <- as.data.frame(x)
    y <- unlist(x[,2])
    z <- unlist(x[,3])
    x <- unlist(x[,1])
  }
  
  p.lab <- par("lab")
  
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle >3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2) 1 - angle else angle - 3)
  
  
  # x axis range
  x.range <- range(x[is.finite(x)], xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 *lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  x <- x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  
  # y axis range
  y.range <- range(y[is.finite(y)], ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 *lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  y <- (y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim))
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  
  # Z axis range
  z.range <- range(z[is.finite(z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 *lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  z <- z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  
  # Add grid
  if ("xy" %in% grid || grid == TRUE) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max + 
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
  
  if ("xz" %in% grid) {
    i <- x.min:x.max
    segments(i + (yx.f * y.max), yz.f * y.max + z.min, 
             i + (yx.f * y.max), yz.f * y.max + z.max, 
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.max + temp,temp1 + i , col = col.grid, lty = lty.grid)
    
  }
  
  if ("yz" %in% grid) {
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min,  
             x.min + (i * yx.f) ,i * yz.f + z.max,  
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.min, i , col = col.grid, lty = lty.grid)
  }
  
}

plotTestBenchResults <- function(df,envir = parent.frame()){
  df_avgs <-
    setDT(
      melt(
        data = copy(df)[, `:=`(
          mean_server_utilization = (100 * mean_server_utilization / .SD[Iteration == 0, mean_server_utilization]) -
            100,
          max_mean_queue = (100 * max_mean_queue / .SD[Iteration == 0, max_mean_queue]) - 100,
          avg_wait = (100 * avg_wait / .SD[Iteration == 0, avg_wait]) - 100 ,
          Dist = signif(Dist, digits = 4)
        ), by = instance], 
        measure.vars = colnames(best$Obj_mean)
      )
    )[,.(value = mean(value),sd = sd(value)),by = list(Iteration,variable)]
  
  p <- ggplot(data = df_avgs,
              mapping = aes(x = Iteration, y = value, colour = variable)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = value - sd,
                      ymax = value + sd),
                  width = .2,
                  position = position_dodge(0.05))
  return(p)
}

# Convenience Functions ---------------------------------------------------
extractParetoSets <- function(directory,envir = parent.frame()) {
  pareto_sets <- lapply(
    X = seq(length(list.files(
      file.path(directory,
                "Inverted_V_Full_Environments")
    ))),
    FUN =  function(i) {
      load(file.path(
        directory,
        "Inverted_V_Full_Environments",
        paste0('Trial_', i, '.Rdata')
      ))
      return(pareto_set)
    }
  )
}

candidateSettoMatrix <- function(set,attr,envir = parent.frame()){
  attr <- if(!is.character(attr)) deparse(substitute(attr)) else attr
  return(t(matrix(unlist(set %c% attr),ncol = length(set))))
}

specify_decimal <- function(x, digits) as.numeric(trimws(format(round(x, digits), nsmall=digits))) 
>>>>>>> 315b489 (Repo structure changes: Removed the MOSA Fucntions.R file and moved all functions into a separate "Functions" folder.)
