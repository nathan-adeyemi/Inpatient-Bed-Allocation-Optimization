mh_wait_quantile <- function(x,quant = 0.9){
  return(x[,.(wait_90_quantile = quantile(total_wait_time,probs = quant,na.rm = T)),by = replication])
}