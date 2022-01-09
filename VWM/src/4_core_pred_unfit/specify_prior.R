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
sig_scale <- c(0.05,0.1,0.5,1)
post_param <- read_csv('./VWM/output/results/fit_prev/param_im.csv')
mu_post <- signif(post_param$mean,2)
mu_post

## a ---------------
mu <- mu_post[1]; lb <- 0
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.087,lb,ub)
sd(x)
#0.008 0.016 0.087 0.22

# b ---------------
mu <- mu_post[2]; lb <- 0
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.21,lb,ub)
sd(x)
#0.0075 0.0150 0.081 0.2

# r ---------------
mu <- mu_post[3]; lb <- 0; ub <- 1
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.17,lb,ub)
sd(x)

#0.0065 0.0130 0.07 0.18

# kappa ----------------
mu <- mu_post[5]; lb <- 0; ub <- 30
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,21,lb,ub)
sd(x)
#0.42 0.83 4.55 20

# delta --------------
mu <- mu_post[6]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,5.4,lb,ub)
sd(x)
# 0.5  1.0  5.4 13.5