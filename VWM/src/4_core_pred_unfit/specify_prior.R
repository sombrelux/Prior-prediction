rm(list=ls())
library(tidyverse)

# sd2k ------------
sd2k <- function(s){
  R = exp(-s^2/2)
  k = 1/(R^3-4*R^2+3*R)
  if((0.53<=R) & (R<0.85)) k = -0.4+1.39*R+0.43/(1-R)
  if(R < 0.53) k = 2*R+R^3+(5*R^5)/6
  return(k)
}
sd2k(0.29+3*0.03) #7.46
sd2k(0.29-3*0.03) #25.51

sd2k(0.6-3*0.04) #4.89
sd2k(0.6+3*0.04) #2.56

sd2k(22.2/180*pi) #7.19
sd2k(9.1/180*pi) #40.15

# s ---------
exp(-10)#~e-5
exp(-20)#~e-9
exp(-30)#~e-14

# std -----------
rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 10000000

post_param <- read_csv('./VWM/output/results/fit_prev/param_im.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

## a ---------------
mu <- 0.16; lb <- 0; ub <- 1
a <- rtruncnorm(n,mu,0.11,lb,ub)
sd(a)
#0.001,0.005,0.01,0.05,0.12

# b ---------------
mu <- 0.15; lb <- 0; ub <- 1
b <- rtruncnorm(n,mu,0.12,lb,ub)
sd(b)
#0.001,0.005,0.01,0.05,0.12

# r ---------------
mu <- 0.13; lb <- 0; ub <- 1
r <- rtruncnorm(n,mu,100,lb,ub)
sd(r)
#0.01,0.05,0.1,0.5,1
#0.01,0.051,0.13,100,100

# s ---------------
mu <- 4.7; lb <-0 ; ub <- 10
s <- rtruncnorm(n,mu,1000,lb,ub)
sd(s)
#0.1, 0.5, 1, 5,10
#0.1,0.5,1,100,100
#30,5,8.15,
# kappa ----------------
mu <- 8.3; lb <- 0; ub <- 30
kappa <- rtruncnorm(n,mu,1000,lb,ub)
sd(kappa)
# 0.1, 0.5, 1, 5,10
#0.1,0.5,1,5.75,100

# delta --------------
mu <- 10; lb <- 0; ub <- 1000000
delta <- rtruncnorm(n,mu,13.43,lb,ub)
sd(delta)
#0.1,0.5,1,5,10
#0.1,0.5,1,5.41,13.43

# w ---------------
w <- rbeta(n,2,1)
hist(w)
