objective_Metrics_nsga2 <- function(data,fun_list = NA,.envir = parent.frame()){
  data %>% 
    {function(data) lapply(fun_list,FUN = function(i) eval(parse(text = paste0(i,'(data)'))))}() %>%
    Reduce(merge,.) %>% data.table()
}
