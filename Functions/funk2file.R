func2file <- function(FUN){
  func_file <- file(file.path('Functions',paste(FUN,'R',sep = '.')),open = 'wt')
  sink(func_file)
  tryCatch({
    do.call(what = FUN,
            args = list(2))
  }, error = function(e) {
    #Do nothing
  })
  sink(func_file)
}