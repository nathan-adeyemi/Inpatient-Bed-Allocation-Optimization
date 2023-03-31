p_k_calc <- function(j,k){
  if (k <= queues_df[j,server_count]) {
    return(queues_df[j,p_0] * (queues_df[j,rho] ^ k) / factorial(k))
  } else {
    return(queues_df[j,p_0] * (queues_df[j,rho] ^ k) /
             (factorial(queues_df[j,server_count]) *
                queues_df[j,server_count] ^ (k - queues_df[j,server_count])))
  }
}