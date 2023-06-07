build_itReps_df <- function(history, cumulative = F,plot_replication_info = F,.envir = parent.frame()) {
  if(plot_replication_info){
    cumulative <- T
  }
  itRepsDF <- lapply(
    X = seq_along(history),
    FUN = function(it,history_list) {
      if (it == 1) {
        calc_reps <-
          sum(unlist(history_list[[it]], recursive = F) %c% 'Replications') + .envir$initial_trials
        new_pareto_solns <- history_list[[1]]$itBest
      } else {
        new_pareto_solns <- history_list[[it]]$itBest[which(history_list[[it]]$itBest %c% 'name' != history_list[[it -1]]$itBest %c% 'name')]
        new_pareto_solns <- new_pareto_solns[!sapply(new_pareto_solns,is.null)]
        calc_reps <-
          sum(`if`(length(new_pareto_solns) > 0, new_pareto_solns  %c% 'Replications',0),
              history_list[[it]]$Rejects %c% 'Replications')
      }
      dt <- data.table(
        Iteration = it,
        DD_PUSA_Reps = calc_reps,
        Theoretical_Reps = length(append(new_pareto_solns,history_list[[it]]$Rejects)) * 20,
        Temperature = cool_temp(current_iteration = it, .envir = .envir),
        NSGA_II_Reps = 20 * 50
      )
      return(dt)
    },
    history_list = history
  )
  itRepsDF <- rbindlist(itRepsDF)
  if (cumulative) {
    itRepsDF <- melt(itRepsDF, id.vars = 'Iteration')[melt(itRepsDF, id.vars = 'Iteration'), .(value = sum(x.value, na.rm = T)), on = .(variable, Iteration <= Iteration), by = .EACHI]
  }
  if(!plot_replication_info){  
    return(itRepsDF)
  } else {
    itRepsDF <-
      itRepsDF[variable == 'DD_PUSA_Reps', variable := 'DD-PUSA w/ MOCBA'
               ][variable == 'Theoretical_Reps', variable := 'DD-PUSA w/o MOCBA'
                 ][variable == 'NSGA_II_Reps', variable := 'NSGA-II']
    ggplot(data = itRepsDF[variable != 'Temperature', ],
           mapping =  aes(x = Iteration, y = value, group = variable)) +
      geom_bar(stat = 'identity', aes(fill = variable),position =  position_stack(reverse = T),width = 0.5) +
      #geom_line(stat = 'identity',aes(colour = variable)) +
      ylab('# of Simulation Replications') +
      xlab('Iteration/Generation') +
      ggtitle('A Comparison of Simulation Replications used by \n Different Solution Algorithms') + 
      labs(fill = 'Algorithm') +
      theme(legend.position = 'bottom') 
  }
}