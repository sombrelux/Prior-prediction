rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 10000000

# HD ----------
## a ==========
mu <- 0.5; lb <- 0; ub <- 2
a <- rtruncnorm(n,mu,0.1,lb,ub)
sd(a)

#0.01: 0.01000478
#0.05: 0.0500151
#0.1: 0.09999317

## logh =======
mu <- -2; lb <- -1000000; ub <- 0
logh <- rtruncnorm(n,mu,2.69,lb,ub)
sd(logh)
#0.5: 0.499886
#1.08: 0.9993289
#2.69: 2.002979

## i ===============
mu <- 1.5; lb <- 0; ub <- 1000000
i <- rtruncnorm(n,mu,2.85,lb,ub)
sd(i)
# 0.504: 0.5004566
#1.19: 0.9999727
#2.85: 2.000769
## s ===========
mu <- 1; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,3.01,lb,ub)
sd(s)
#0.54: 0.5000276
#1.34: 0.99849
#3.01: 2.00166

# MHD ----------

# PTT ----------
