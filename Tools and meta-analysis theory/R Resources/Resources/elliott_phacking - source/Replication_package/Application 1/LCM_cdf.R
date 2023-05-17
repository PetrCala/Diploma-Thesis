#####################################################################
# This function generates BB functionals used to calculate critical values for LCM tests
# Paper: Detecting p-hacking
# Authors: G. Elliott, N. Kudrin, K. Wuthrich
#####################################################################
library("fdrtool")
RNGkind(sample.kind = "default")
set.seed(123)

#Simulate cdfs for LCM tests
M = 10000
N = 10000
c = 1:N
c = c/N
#BB = matrix(0, nrow=M, ncol = 1)
BBsup = matrix(0, nrow=M, ncol = 1)
for (m in 1:M){
  eps <- rnorm(n = N, sd = 1, mean = 0)
  eps = eps/sqrt(N)
  W = cumsum(eps)
  B = W - c*W[N]
  C = c(0,c)
  B = c(0,B)
  ll = gcmlcm(C,B, type="lcm")
  y = 0
  for (s in 2:length(ll$x.knots)){
    a = ll$y.knots[s] - ll$slope.knots[s-1]*ll$x.knots[s]
    b = ll$slope.knots[s-1]
    xl = ll$x.knots[s-1]*N+1
    xu = ll$x.knots[s]*N
    xx = xl:xu
    xx = xx/N
    yy = a + b*xx
    y = c(y,yy)
  }
  #BB[m,1] = sqrt(mean((abs(y - B)^2)))
  BBsup[m,1] = max(abs(y - B))
}

#write.csv(BB, file = "BB.csv")
write.csv(BBsup, file = "Replication_package/Application 1/BB_sup.csv")