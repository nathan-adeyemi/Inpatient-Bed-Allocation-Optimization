ci_as_text <- function(interval,dec.digits = 3){
  interval <- specify_decimal(interval,digits = dec.digits)
  paste0("(",min(interval),",",max(interval),")")
}
