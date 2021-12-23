rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 10000000
# a, b ---------------
mu <- 0; lb <- 0; ub <- 10000000
a <- rtruncnorm(n,mu,0.166,lb,ub)
sd(a)
#0.0166: 0.01000212
#0.017: 0.01025133

#0.082: 0.04941343
#0.083: 0.05001778

#0.165: 0.09947038
#0.166：0.1001079

# r ---------------
mu <- 0; lb <- 0; ub <- 1
r <- rtruncnorm(n,mu,0.166,lb,ub)
sd(r)
#0.0166: 0.01000789
#0.083: 0.05003451
#0.166：0.10005

# kappa ----------------
mu <- 8; lb <- 0; ub <- 1000000
kappa <- rtruncnorm(n,mu,2,lb,ub)
sd(kappa)
# 0.5: 0.4999318
#1: 1.000005
#2: 1.999117

# delta --------------
mu <- 10; lb <- 0; ub <- 1000000
delta <- rtruncnorm(n,mu,0.5,lb,ub)
sd(delta)
# 0.5: 0.4999683
#1: 0.9997557
#2: 2.000271

# s ---------------
mu <- 5; lb <-0 ; ub <- 10000000
s <- rtruncnorm(n,mu,2.05,lb,ub)
sd(s)
#0.5: 0.5000126
#1: 1
#2.05: 1.997865
