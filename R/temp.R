temp <- function(){
  M = 100
  N = 100
  c = 1:N
  c = c/N
  BBsup = matrix(0, nrow=M, ncol = 1)
  
  for (m in 1:M){
    eps <- rnorm(n = N, sd = 1, mean = 0)
    eps = eps/sqrt(N)
    W = cumsum(eps)
    B = W - c*W[N]
    C = c(0,c)
    B = c(0,B)
    ll = gcmlcm(C,B, type="lcm")
    
    # Preallocate y vector with the same length as C
    y = numeric(length(C))
    y[1] = 0
    
    for (s in 2:length(ll$x.knots)){
      a = ll$y.knots[s] - ll$slope.knots[s-1]*ll$x.knots[s]
      b = ll$slope.knots[s-1]
      xl = ll$x.knots[s-1]*N+1
      xu = ll$x.knots[s]*N
      xx = xl:xu
      xx = xx/N
      yy = a + b*xx
      
      # Fill in the y vector with the computed yy values
      y[xl:xu] = yy
    }
    BBsup[m,1] = max(abs(y - B))
  }
  cdfs <- BBsup[,1] # To a numeric vector
}

debug(temp)
temp()
