# OLD AND NEW FUNCTIONS

# Original function for weighted mean of squared matrixes
weighted_mean_squared_source <- function(beta, se, sigma){
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  for (i in 2:N){
    Y1 <- sum(WB[1:i,1:i]) - sum(weights_beta[1:i]^2)
    Y2 <- sum(W[1:i,1:i]) - sum(weights[1:i]^2)
    Y[i] <- Y1/Y2
  }
  return(Y)
}

# New function for calculating partial matrix sums
compute_submat_sums <- function(mat) {
  # calculate cumulative sums over rows and columns
  f <- function(x){cumsum(as.numeric(x))}
  sum1 <- apply(mat, 2, f) # Sum over cols
  sum2 <- apply(sum1, 1, f) # Sum over rows
  out <- diag(sum2)
  return(out)
}

# New function for weighted mean of squared matrixes
weighted_mean_squared_new <- function(beta, se, sigma){
  N <- length(beta)
  Y <- vector(mode = 'numeric', length = N)
  
  weights <- 1/(se^2 + sigma^2)
  weights_beta <- weights*beta
  
  W <- weights %o% weights
  WB <- weights_beta %o% weights_beta
  
  W_csum <- compute_submat_sums(W)
  WB_csum <- compute_submat_sums(WB)
  
  # Create cumulative sum of weights_beta squared - works fine
  weights_beta_csum <- cumsum(weights_beta^2)
  weights_csum <- cumsum(weights^2)
  
  # Preallocate vector
  Y1 <- numeric(N - 1)
  Y2 <- numeric(N - 1)
  
  # Perform the operation
  Y1 <- WB_csum - weights_beta_csum
  Y2 <- W_csum - weights_csum
  Y <- Y1 / Y2
  Y[1] <- 0
  
  return(Y)
}