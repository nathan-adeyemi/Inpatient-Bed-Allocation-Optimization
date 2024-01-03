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