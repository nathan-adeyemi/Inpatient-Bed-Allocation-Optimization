read_temp_files <- function(folder){
  pop <- list()
  
  for (a in list.files(folder)){
    temp <- readRDS(file.path(folder,a))
    pop <- append(pop,temp$Previous_Best)
    temp <- NULL
  }
  pop <- pop[!duplicated(pop)]
  return(pop)
}