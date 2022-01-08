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

# HD ----------
post_param <- read_csv('./RIC/output/results/fit_pilot/HD_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post

## a ==========
mu <- mu_post[1]; lb <- 0; ub <- 2
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,100,lb,ub)
sd(x)

#0.03,0.061,0.33,1000

## logh =======
mu <- mu_post[2]; lb <- -1000000; ub <- 0
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,4.7,lb,ub)
sd(x)
#0.18,0.35,2,4.7

## i ===============
mu <- mu_post[3]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,65.8,lb,ub)
sd(x)
#2.4,4.9,25.8,65.9

## s ===============
mu <- mu_post[4]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.32,lb,ub)
sd(x)
#0.012,0.024,0.13,0.33

# MHD ----------
post_param <- read_csv('./RIC/output/results/fit_pilot/MHD_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post 

## a ============
mu <- mu_post[1]; lb <- 0; ub <- 2
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.29,lb,ub)
sd(x)
#0.025,0.05,0.28,0.86

## c ================
mu <- mu_post[2]; lb <- 0; ub <- 1
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.2,lb,ub)
sd(x)
#0.018,0.037,0.2,1000

## loghd ============
mu <- mu_post[3]; lb <- -1000000; ub <- 0
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,2.83,lb,ub)
sd(x)
#0.1,0.2,1.1,2.83

## loghr ============
mu <- mu_post[4]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.86,lb,ub)
sd(x)
#0.034,0.067,0.37,0.85

## sd ============
mu <- mu_post[5]; lb <- 0; ub <- 1
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.41,lb,ub)
sd(x)
#0.012,0.025,0.13,0.4

## sr ==========
mu <- mu_post[6]; lb <- 0; ub <- 1
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.34,lb,ub)
sd(x)
# 0.012,0.023,0.13,0.34

## s ==============
mu <- mu_post[7]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.42,lb,ub)
sd(x)
#0.016,0.031,0.18,0.42

# PTT ----------
post_param <- read_csv('./RIC/output/results/fit_pilot/PTT_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post 

## alpha ========
mu <- mu_post[1]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.11,lb,ub)
sd(x)
# 0.0038,0.0077,0.041, 0.11

## beta ==========
mu <- mu_post[2]; lb <- 0; ub <- 1
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.6,lb,ub)
sd(x)
# 0.032,0.063,1000,1000

## gamma ========
mu <- mu_post[3]; lb <- 0; ub <- 1
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.11,lb,ub)
sd(x)
# 0.045,0.11,1000,1000

## R =========
mu <- mu_post[4]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,1.9,lb,ub)
sd(x)
#0.07,0.14,0.76,1.9

## s =======
mu <- mu_post[5]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,1.25,lb,ub)
sd(x)
#0.047,0.093,0.5,1.25

# RITCH ------------
post_param <- read_csv('./RIC/output/results/fit_pilot/RITCH_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post

## mu_beta_xt =======
mu <- mu_post[1]
sig_post <- mu*sig_scale
signif(sig_post,2)

## mu_beta_xp =======
mu <- mu_post[2]
sig_post <- mu*sig_scale
signif(sig_post,2)

## mu_beta_xa =======
mu <- mu_post[3]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,1.5*10^(-3),lb,ub)
sd(x)
#5.5e-05 1.1e-04 6e-04 1.5e-03

## mu_beta_xr ====== 
mu <- mu_post[4]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.64,lb,ub)
sd(x)
#0.024 0.048 0.26 0.65

## mu_beta_pa ==========
mu <- mu_post[5]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.64,lb,ub)
sd(x)
# 0.024 0.048 0.26 0.65

## mu_beta_pr ======= 
mu <- mu_post[6]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.72,lb,ub)
sd(x)
#0.027 0.054 0.3 0.73

## mu_beta_ta ========
mu <- mu_post[7]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.037,lb,ub)
sd(x)
# 0.0014 0.0028 0.015 0.038

## mu_beta_tr ========
mu <- mu_post[8]; lb <- 0; ub <- 1000000
sig_post <- mu*sig_scale
signif(sig_post,2)
x <- rtruncnorm(n,mu,0.17,lb,ub)
sd(x)
#0.0065 0.0130 0.07 0.18
