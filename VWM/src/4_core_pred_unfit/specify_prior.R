rm(list=ls())
library(tidyverse)

rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 10000000

post_param <- read_csv('./VWM/output/results/fit_prev/param_IM.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

# a ---------------
mu <- 0.16; lb <- 0; ub <- 1000000
a <- rtruncnorm(n,mu,0.05,lb,ub)
sd(a)
#0.001,0.005,0.01,0.05

# b ---------------
mu <- 0.15; lb <- 0; ub <- 1000000
b <- rtruncnorm(n,mu,0.0504,lb,ub)
sd(b)
#0.001,0.005,0.01,0.05

# r ---------------
mu <- 0.13; lb <- 0; ub <- 1
r <- rtruncnorm(n,mu,100,lb,ub)
sd(r)
#0.01,0.05,0.1,0.5
#0.01,0.051,0.13,100

# s ---------------
mu <- 4.7; lb <-0 ; ub <- 10000000
s <- rtruncnorm(n,mu,1,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.1,0.5,1,6.81

# kappa ----------------
mu <- 8.3; lb <- 0; ub <- 1000000
kappa <- rtruncnorm(n,mu,1,lb,ub)
sd(kappa)
# 0.1, 0.5, 1, 5
#0.1,0.5,1,5.74

# delta --------------
mu <- 10; lb <- 0; ub <- 1000000
delta <- rtruncnorm(n,mu,1,lb,ub)
sd(delta)
#0.1,0.5,1,5
#0.1,0.5,1,5.41