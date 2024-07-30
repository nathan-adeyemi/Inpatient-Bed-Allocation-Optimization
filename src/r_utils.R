get_sys_env_var <- function(var_name, var_class = 'numeric') {

  # Function that grabs relecant environment variables from the encompassing shell session.
  # Environment variables are added by the parent python process 

  res <- Sys.getenv(var_name)
  if (nchar(res) == 0) {
    return(NULL)
  } else if(grepl('vector',var_class)){
    return(unlist(fromJSON(res)))
  } else if (var_class == 'numeric') {
    return(as.numeric(res))
  } else if (grepl("integer",var_class)){
    return(as.integer(res))
  } else if (grepl('datatable|dataframe', var_class)) {
    return(data.table(fromJSON(res)))
  } else if (grepl("bool|logic",var_class)){
    return(as.logical(res))
  } else {
    return(as.character(res))
  }
}

transmit_results <-  function(results,receiver) {
    write.socket(receiver, jsonlite::toJSON(results, auto_unbox = TRUE))
  }

read_json_con <- function(socket, max_length = 256){
  json_string <- ""
  
  while(TRUE){
    new_piece <- read.socket(socket)
    json_string <- paste0(json_string, new_piece)
    if(nchar(new_piece) < max_length){
      break
    }
  }
  return(json_string)
}
