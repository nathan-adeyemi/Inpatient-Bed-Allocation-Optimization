paretoSetBoxplot <- function(pSet1,pSet2,initial_sol,measure){
  # pSet1 <- rbindlist(lapply(pSet1,function(soln){
  #   with(soln,
  #        data.table(Cost))
  # }))[,`Problem Configuration` := 'Configuration 1']
  # pSet2 <- rbindlist(lapply(pSet2,function(soln){
  #   with(soln,
  #        data.table(Cost))
  # }))[,`Problem Configuration` := 'Configuration 2']
  
  pSet1 <- rbindlist(lapply(pSet1,function(i) i$Cost))[,`Problem Configuration` := 'Configuration 1']
  pSet2 <- rbindlist(lapply(pSet2,function(i) i$Cost))[,`Problem Configuration` := 'Configuration 2']
  inputData <- rbind(pSet1,pSet2,copy(initial_sol$Cost)[,`Problem Configuration` := 'Initial Allocation Results'],fill = T)
  ggplot(data = melt(inputData,id.vars = 'Problem Configuration')[variable == measure,],aes(variable,value)) + 
    geom_boxplot(aes(fill = `Problem Configuration`)) #+ 
    #geom_point(data = melt(initial_sol$Obj_mean)[variable == measure,],aes(variable,value))
}