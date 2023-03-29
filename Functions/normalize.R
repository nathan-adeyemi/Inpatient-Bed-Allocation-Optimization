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