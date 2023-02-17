smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] %+% 1
  return(y)
}

norm_vec <- function(x) {
  x / sum(x)
}

decode <- function(alg_input,test_bench = get('use_test_bench',envir = .GlobalEnv)){
  if (!test_bench) {
    alg_input <-
      unique(copy(siteInfo
      )[, list(Facility_name, Bed_Group, total_beds)]
      )[, inputs := alg_input
      ][, `:=`(Sums = sum(inputs),
               fac_beds = sum(total_beds)), 
        by = Facility_name
      ][, bed_counts := {Vectorize(smart.round)}(inputs / Sums * fac_beds)]
    return(alg_input$bed_counts)
  } else {
    new_alloc <- alg_input %>% 
      norm_vec() 
    new_alloc <-
      smart.round(x = new_alloc * total_servers)
    return(new_alloc)
  }
}

# Functions for Multi-Objective Simulated Annealing -----------------------
termination_criteria <-
  function(eval_gradient = F,
           check_pareto = F,
           check_iteration = F,
           iteration = get('it', envir = .GlobalEnv)) {
    obj_names <- colnames(best$Obj_mean)
    if (iteration < 1) {
      logical <- T
    } else if (eval_gradient) {
      
      # Defines the minimum gradient stopping criteria
      grad_term <- data.table(matrix(c(0,0.5,0,-0.05,0,-0.05),nrow = 2))
      
      #Names the gradient terminating criteria dataframe columns
      colnames(grad_term) <- colnames(best$Obj_mean) 
      
      best_df <- get('best_df', envir = .GlobalEnv)
      gradient <-
        tail(tail(unique(best_df[, ..obj_names]), n = 2)[, lapply(
          X = .SD,
          FUN = function(i) {
            100 * ((i - data.table::shift(i, n = 1)) / data.table::shift(i, n = 1))
          }
        ), .SDcols = obj_names],
        n = 1)
      
      logical <-
        !all(sapply(
          X = obj_names,
          FUN = function(column) {
            return(min(gradient_term[, ..column]) < gradient[, ..column] &
                     max(gradient_term[, ..column]) > gradient[, ..column])
          }
        ) == T)
    } else if (check_pareto) {
      logical <- any(temp > t_min, get('pareto_counter',envir = .GlobalEnv) < pareto_limit)
    } else if (check_iteration){
      logical <- iteration < itMax
    } else {
      logical <- all(temp > t_min, best_counter < best_limit)
    }
    return(logical)
  }

fix_cell <- function(i){
  if (i < 0){
    i <- abs(i)
  } else if (i > 1){
    i <- (1-i) + 1
  }
  i
}

tweak <- function(x) {
  p_change <- 1 - (exp(temp - temp_init) / exp(temp))
  changes <- as.numeric(runif(nVar) < p_change)
  changes <-
    runif(n = length(changes),
          min = -maxChange,
          max = maxChange) * changes
  x <- sapply(x + changes, fix_cell)
  return(x)
}

read_temp_files <- function(){
  pop <- list()
  folder <- list.files(file.path(".","Data","MOSA Results",paste0("Temp_Archive_Files_",today))) %>% 
    {function(fl)  file.path(".","Data","MOSA Results",paste0("Temp_Archive_Files_",today), fl[length(fl)])}()
  
  for (a in list.files(folder)){
    temp <- readRDS(file.path(folder,a))
    pop <- append(pop,temp$Previous_Best)
    temp <- NULL
  }
  pop <- pop[!duplicated(pop)]
  return(pop)
}

ndsa <- function(pop){
  allFronts = list()
  frontRank <- 1
  
  while (length(pop) > 1){
    front <- list(pop[[1]])
    popPrime <- pop
    pop <- pop[2:length(pop)]
    front_ind <- c(1) # front_ind is the is current front's solution's index in the overall population of results (pop)
    
    for (i in seq_along(pop)){
      front <- c(front,list(pop[[i]]))
      front_ind <- c(front_ind,i+1)
      end <- length(front)
      frontRemoveIndex <- c() #frontRemoveIndex is the index of a solution in the current front NOT the population
      
      for (j in seq_along(front)[-end]){
        #If the added solution is dominated by any solution in the current front, remove it and break the loop
        paretoTest1 <- soln_comparison(front[[end]],front[[j]]) %>% {function(vec) sum(vec) == length(vec)}()
        
        # If a solution in the current front is dominated by the tested solution, remove that solution in the current front and add the other
        paretoTest2 <- soln_comparison(front[[j]],front[[end]]) %>% {function(vec) sum(vec) == length(vec)}()
        if(paretoTest1){ 
          if(!(end %in% frontRemoveIndex)){
            frontRemoveIndex <- c(frontRemoveIndex,end)
          }
        }else if (paretoTest2){ 
          frontRemoveIndex <- c(frontRemoveIndex,j)
        }
      }
      
      # Remove dominated solutions from current front and front index list
      front[frontRemoveIndex] <- NULL
      if(!is.null(frontRemoveIndex)){
        front_ind <- front_ind[-frontRemoveIndex]
      }
    }
    allFronts[paste0('Front_',frontRank)] <- list(front)
    pop <- popPrime[-front_ind]
    front <- list()
    frontRank <- frontRank %>%  sum(1)
  }
  return(allFronts)
}

rankDF <- function(rank){
  count = 1
  dt <- data.frame(matrix(data = 0,ncol = length(best$Obj_mean),nrow = length(allFronts[[rank]])))
  names(dt) <- names(best$Obj_mean)
  for (i in allFronts[[paste0('Front_',rank)]]){
    dt[count,] <- i$Obj_mean
    count <- count %>%  sum(1)
  }
  dt$rank <- rank
  return(dt)
}

CostFunction <- function(sol = NULL,
                         logic,
                         reps = seq(10),
                         analytical = FALSE,
                         test_bench = get(x = 'use_test_bench', envir = .GlobalEnv),
                         sim_length = get(x = 'sim_length', envir = .GlobalEnv),
                         sim_warmup = get(x = 'warmup', envir = .GlobalEnv),
                         use_inv_V = get(x = 'inverted_V_logical', envir = .GlobalEnv),
                         nsga = F) {
  if(nsga){
    sol <- decode(sol)
  }
  
  if (analytical == T) {
    
  } else if (test_bench == T) {
    x <- run_test_bench(
      rep_nums = reps,
      network_df = copy(queues_df)[, server_count := sol],
      multicore = logic,
      sim_length = sim_length,
      warmup = sim_warmup,
      inverted_V = use_inv_V
    )
  } else{
    x <- data.table(
      full_sim(
        num_iter = reps,
        parallel = logic,
        new_sol = sol,
        warmup = 3,
        sim_length = 35
      )
    )
  }
  if (nsga) {
    x <- apply(objective_Metrics_nsga2(x, fun_list = grep(
      'TB_', lsf.str(envir = .GlobalEnv), value = T
    ))[,-1], 2, mean)
  }
  return(x)
}

update_sol <- function(i,sol_vector = NA){
  x <- best
  if (any(is.na(sol_vector))) {
    x$Replications <- get('starter_reps',envir = .GlobalEnv)
    x$counter <- 0
    x$name <-  paste0('Tourney_', it, '_Candidate_',i)
    x$Solution <-  tweak(x$Solution)
  } else{
    x$name <-
      paste0("test_nsga_sol_",i)
    x$Solution <- sol_vector
    x$Replications <- 12
  }
  x$Allocation <- decode(x$Solution)
  return(x)
}


gen_candidates <- function(tweak_left,candidate_list = NULL) {
  temp_counter <- 0
  new_solns <- list()
  if (length(candidate_list) == 0){
    while (tweak_left > 2 & temp_counter < 25) {
      candidate_list <- lapply(seq(tweak_left), update_sol)
      # Remove duplicate allocations within temporary obj (inital subgroup)
      new_alloc = which({
        function(mat)
          ! duplicated(mat)
      }(t(
        as.matrix(candidate_list %c% 'Allocation')
      )))
      candidate_list = candidate_list[new_alloc]
      
      
      # Remove any solution that was previously tested
      dups <-
        rbind(all_allocations,
              candidate_list %c% 'Allocation' %>%
                t() %>%
                as.matrix()) %>%
        as.matrix() %>%
        {
          function(mat)
            which(duplicated(mat))
        }()
      if (length(dups) > 0) {
        dups <- dups - nrow(all_allocations)
      }
      if (length(dups) != length(candidate_list)) {
        candidate_list <-
          candidate_list[setdiff(seq_along(candidate_list), dups)]
      } else {
        candidate_list = NULL
      }
      
      # Add all new generated solutions to the candidate list
      new_solns = append(new_solns, candidate_list)
      if (length(candidate_list) > 0 & it != 0) {
        all_allocations <<-
          rbind(all_allocations, as.matrix(t(candidate_list %c% 'Allocation')))
      }
      
      temp_counter %+% 1
      tweak_left <- nTweak - length(new_solns)
    }
  }
  # Make lists of all new solutions/allocations (1 entry per replication), a list of the replication #s, 
  # and a list of the allocation's index in the candidate list
  allocation_list <-
    unlist(x = lapply(
      X = candidate_list,
      FUN = function(i) {
        rep(x = list(i$Allocation), times = i$Replications)
      }
    ),
    recursive = F)
  
  replication_list <-
    rep(x = seq(candidate_list[[1]]$Replications),
        times = length(candidate_list))
  
  index_list = sapply(
    seq_along(candidate_list),
    FUN = function(i)
      rep(x = i, times = candidate_list[[i]]$Replications)
  )
  
  if (length(new_solns) == 0 & temp_counter == 10) {
    candidate_list = NULL
  } else{
    test <- mclapply(
      X = seq_along(allocation_list),
      FUN = ocbaUpdate,
      mc.cores = availableCores(),
      arg_list = allocation_list,
      job_list = replication_list
    )
    candidate_list <- lapply(
      X = candidate_list,
      FUN = function(Object) {
        obj_ind = which(candidate_list %c% 'name' == Object$name)
        stats <- rbindlist(test[which(unlist(lapply(
          X = seq_along(candidate_list),
          FUN = function(i)
            rep(x = i, times = Object$Replications)
        )) == obj_ind)])
        Object <-
          updateSimStats(i = Object,
                         data = stats,
                         new_sol = T)
      }
    )
  }
  return(candidate_list)
}

updateSimStats <- function(i,data,new_sol){
  i$Cost <- if(!new_sol) rbind(i$Cost,data) else data
  i$Cost[,replication := seq(nrow(i$Cost))]
  setDT(i$Cost)
  i$Obj_mean <-
    i$Cost[, lapply(.SD, mean), .SDcols = colnames(i$Cost)][, replication := NULL]
  i$Obj_var <-
    i$Cost[, lapply(.SD, sd), .SDcols = colnames(i$Cost)][, replication := NULL]
  i$Obj_CI <-
    i$Cost[, lapply(.SD, function(i)
      ci_as_text(interval = t.test(i, conf.level = 0.95)$conf.int)), .SDcols = colnames(i$Cost)][, replication := NULL]
  i[['addReps']] <- NULL
  i[['deltaPsiD']] <- NULL
  i[['psiID']] <- NULL
  return(i) 
}

objective_Metrics <- function(data,fun_list = NA){
  data %>% 
    {function(data) lapply(fun_list,FUN = function(i) eval(parse(text = paste0(i,'(data)'))))}() %>%
    Reduce(merge,.) %>% data.table()
}

simple_mean_comparison <- function(metric, df, strict = T) {
  if (strict)
    df[soln_num == 2, sapply(.SD, mean), .SDcols = metric] < df[soln_num == 1, sapply(.SD, mean), .SDcols = metric]
  else
    df[soln_num == 2, sapply(.SD, mean), .SDcols = metric] <= df[soln_num == 1, sapply(.SD, mean), .SDcols = metric]
}

soln_comparison <-
  function(s1,
           s2,
           stat = get('stat_logical', envir = .GlobalEnv),
           alpha = 0.1) {
    comp_df <- rbind(copy(s1$Cost)[,soln_num := 1],
                     copy(s2$Cost)[,soln_num := 2])
    
    #Less strict alpha value if there are few simulation replications
    if(min(comp_df[,.N,by = soln_num][,N]) < 12){ 
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

p_accept <- function(factor,diff,temp,gradient = F){
  if(factor == 0){
    # Assign acceptance Probability = 1 if solution dominates best
    return(1)
  }else if(factor == get('num_obj',pos = -1)){
    return(0)
  }else{
    # Acceptance Probability ~ Number of improved objective functions, differene in Psi, Temperature
    return(exp(-(diff^(.25)*factor^1.5))/exp(temp_init-temp))
    # return(-exp(factor * diff^.75)/exp(temp - temp_init))
  }
}

noisyNonDominatedSorting <- function (inputData) 
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
        if (soln_comparison(s1 = xj,s2 = xi,stat = T)) { 
          # Test if xi dominates xj
          idxDominators[[j]] = c(idxDominators[[j]], 
                                 i)
          idxDominatees[[i]] = c(idxDominatees[[i]], 
                                 j)
        }
        else if (soln_comparison(s1 = xi,s2 = xj,stat = T)) {
          # Test if xj dominates xi
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
  #browser()
  
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

updateParetoSet <- function(pSet,candidate_list){
  n_obj <- length(optim_type)
  pSet <-  append(pSet,candidate_list)
  pSet <- removeDuplicateSolutions(pSet)
  ranks <-
    noisyNonDominatedSorting(inputData = pSet)
  
  pSet <- pSet[ranks$rnkList[[1]]]
  pSet <- lapply(pSet,procedureI_func,all_candidates = pSet)
  return(list('pSet' = pSet, 'ranks' = ranks))
}

findBestbyDistance <- function(pSet, rankInfo) {
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
    
    g_ideal_CI <<- apply(g_ideal_dist, 2, ci_as_text)
    
    if (length(pSet) == 1) {
      best <- pSet[[1]]
    } else{
      distances_dist <- sapply(
        X = pSet,
        FUN = function(i)
          cramer::cramer.test(x = as.matrix(i$Cost)[, -1], y = g_ideal_dist)$statistic
      )
      
      
      if (all(it > best_limit, 
              best_counter >= best_limit)){
              #length(best_df[(it - min(best_limit, it - 1)):it, unique(Dist)]) == 1)){
        best_by_min_dist <<- !best_by_min_dist
        best_counter <- 0
      }

      if (best_by_min_dist) {
        best <- pSet[[which.min(distances_dist)]]
      } else{
        best <- pSet[[which.max(distances_dist)]]
      }
    }
  } else{
    g_ideal_CI <<- unlist(best$Obj_CI)
    best <- pSet[[1]]
  }
  #best <- pareto_set[[sample(seq_along(pareto_set),size = 1)]]
  return(best)
}

findBestbyDominanceMatrix <-
  function(current_best, candidate_list, stat_comp = F) {
    
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
                                           stat_comp = get('stat_logical',envir = .GlobalEnv)) {
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

findBestbyPsi <- function(candidate_list){
  candidate_list <- lapply(candidate_list,procedureI_func,all_candidates = candidate_list)
}

updateHistory <- function(pSet, candidate_list, history) {
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

removeDuplicateSolutions <- function(front){
  return(front[!duplicated(lapply(
    X = front,
    FUN = function(i) i$name))])
}

compareIterationFronts <- function(curr_front,prev_front){
  return(identical(
    removeDuplicateSolutions(curr_front) %c% 'name',
    removeDuplicateSolutions(prev_front)  %c% 'name'
  ))
}

cool_temp  <-
  function(initial_temperature,
           alpha,
           current_iteration,
           exponential = T,
           linear = F,
           log_mult = F,
           quad_cool = F) {
    if (linear) {
      temp <-  initial_temperature / (1 + (alpha * current_iteration))
    } else if (log_mult) {
      temp <-  initial_temperature / (1 + alpha * (log(1 + current_iteration)))
    } else if (quad_cool) {
      temp <- initial_temperature / (1 + (alpha * current_iteration ^ 2))
    } else {
      temp <-  initial_temperature * alpha ^ (current_iteration)
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
                            envir = .GlobalEnv)) {
    
    # Multi-Objective Optimal Computing Budget Allocation 
    N_Total <<- 20 * length(candidate_list)
    psiTarget <- 5.6e-6 #Value from MOCBA Paper (Lee et. al. 2004)
    # psiTarget <- -1
    delta <- get('delta',envir = .GlobalEnv)
    first_time = TRUE
    N_replicated <- sum(candidate_list %c% 'Replications') - (candidate_list[length(candidate_list)] %c% 'Replications')
    K <- max(floor(length(candidate_list) / 3), 4)
    K <- if(length(candidate_list) < K) length(candidate_list) else K 
    
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
        d$deltaPsiD <- ifelse(test = length(d$psiID) > 1,
                              yes = sum(d$psiID[sP_indices]),
                              no = abs(min(0, d$psiID))) # Do we include all delta psiD or just the delta psi_i
        d
      })
      
      sP <- lapply(order(sP %c% 'deltaPsiD',decreasing = T), FUN = function(i) sP[[i]])
      
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
    }
    itReps <<- N_replicated
    return(candidate_list)
  } 

# Function for implementing the NSGA-II Algorithm -------------------------
objective_Metrics_nsga2 <- function(data,fun_list = NA){
  data %>% 
    {function(data) lapply(fun_list,FUN = function(i) eval(parse(text = paste0(i,'(data)'))))}() %>%
    Reduce(merge,.) %>% data.table()
}

sim_nsga2 <- function(inp_vec) {
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

analytical_nsga2 <- function(inp_vec){
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
          MuDistIdx = 10) 
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
normalize <- function(base,df){
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

plotParetoFront <- function(inputData){
  if(any(class(inputData) == 'data.table')){
    inputData <- as.matrix(inputData)
  } else if(class(inputData) == 'list'){
    inputData <- candidateSettoMatrix(set = inputData,attr = 'Obj_mean')
  }
  paretoPlot <- scatterplot3d(inputData, grid = F)
  addgrids3d(inputData, grid = c("xy", "xz", "yz"))
  paretoPlot$points3d(inputData, pch = 16, type = 'h')
}

# Adds grids for scatterplot3d plots
addgrids3d <- function(x, y=NULL, z=NULL, grid = TRUE,
                       col.grid = "grey", lty.grid = par("lty"),
                       lab = par("lab"), lab.z = mean(lab[1:2]),
                       scale.y = 1, angle = 40,
                       xlim=NULL, ylim=NULL, zlim=NULL){
  
  
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

plotTestBenchResults <- function(df){
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
extractParetoSets <- function(directory) {
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

candidateSettoMatrix <- function(set,attr){
  attr <- if(!is.character(attr)) deparse(substitute(attr)) else attr
  return(t(matrix(unlist(set %c% attr),ncol = length(set))))
}

specify_decimal <- function(x, digits) as.numeric(trimws(format(round(x, digits), nsmall=digits)))
