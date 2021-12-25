rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 10000000
# a, b ---------------
mu <- 0; lb <- 0; ub <- 1000000
a <- rtruncnorm(n,mu,0.83,lb,ub)
sd(a)
#0.083: 0.050
#0.166：0.100
#0.33: 0.199
#0.5: 0.301
#0.83:0.500

# r ---------------
mu <- 0; lb <- 0; ub <- 1
r <- rtruncnorm(n,mu,100,lb,ub)
sd(r)
#0.083: 0.05003451
#0.166：0.10005
#0.34: 0.200
#100: 0.289

# kappa ----------------
mu <- 8; lb <- 0; ub <- 1000000
kappa <- rtruncnorm(n,mu,5.81,lb,ub)
sd(kappa)
# 0.5: 0.4999318
#1: 1.000005
#2: 1.999117
#3.05: 3.00
#5.82:5.00
# delta --------------
mu <- 10; lb <- 0; ub <- 1000000
delta <- rtruncnorm(n,mu,5.41,lb,ub)
sd(delta)
# 0.5: 0.4999683
#1: 0.9997557
#2: 2.000271
#3: 2.99
#5.4: 5.00

# s ---------------
mu <- 5; lb <-0 ; ub <- 10000000
s <- rtruncnorm(n,mu,6.71,lb,ub)
sd(s)
#0.5: 0.5000126
#1: 1
#2.05: 1.997865
#3.44: 3.00
#6.71:5.000