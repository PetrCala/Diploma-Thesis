# OLD AND NEW FUNCTIONS

# Original function for weighted mean
weighted_mean_source <- function(beta, se, sigma){
  N_study <- length(beta)
  Y <- vector(mode = 'numeric', length = N_study)
  
  proportional_weights <- 1/(se^2 + sigma^2)
  
  for (i in 1:N_study){
    Y[i] <- beta[1:i] %*% proportional_weights[1:i]/sum(proportional_weights[1:i])
  }
  return(Y)
}

# New function for weighted mean
weighted_mean_new <- function(beta, se, sigma){
  N_study <- length(beta)
  Y <- vector(mode = 'numeric', length = N_study)
  
  proportional_weights <- 1/(se^2 + sigma^2)
  
  for (i in 1:N_study){
    Y[i] <- beta[1:i] %*% proportional_weights[1:i]/sum(proportional_weights[1:i])
  }
  return(Y)
}