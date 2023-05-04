<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a420328 (Git Repo updates)
mh_wait_quantile <- function(x,transfer_only = T,quant = 0.9){
  if(transfer_only){
    x <- x[type == 'Transfer',]
  }
<<<<<<< HEAD
=======
mh_wait_quantile <- function(x,quant = 0.9){
>>>>>>> f4d354d (Further Updates:)
=======
>>>>>>> a420328 (Git Repo updates)
  return(x[,.(wait_90_quantile = quantile(total_wait_time,probs = quant,na.rm = T)),by = replication])
}