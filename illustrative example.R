n <- 100
N <- 100000

resp_real <- resp_vague <- matrix(nrow=n,ncol=N)
for(i in 1:n){
  a_real <- runif(N,0,1)
  a_vague <- runif(N,0,100)
  prob_real <- 1/(1+exp(-a_real))
  prob_vague <- 1/(1+exp(-a_vague))
  resp_real[i,] <- sapply(prob_real, function(u) rbinom(1,1,u))
  resp_vague[i,] <- sapply(prob_vague, function(u) rbinom(1,1,u))
}

prop_real <- colMeans(resp_real)
prop_vague <- colMeans(resp_vague)

