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

# HD ----------
post_param <- read_csv('./RIC/output/results/fit_pilot/HD_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

## a ==========
mu <- mu_post[1]; lb <- 0; ub <- 2
a <- rtruncnorm(n,mu,100,lb,ub)
sd(a)
#0.5, 1, 5, 10
#0.8,100,100

## logh =======
mu <- mu_post[2]; lb <- -1000000; ub <- 0
logh <- rtruncnorm(n,mu,15.5,lb,ub)
sd(logh)
#0.5, 1, 5, 10
#0.5,1,7.2,15.5

## i ===============
mu <- mu_post[3]; lb <- 0; ub <- 1000000
i <- rtruncnorm(n,mu,150.8,lb,ub)
sd(i)
#5, 10, 50, 100
#5,10,67.5,150.7

## s ===============
mu <- mu_post[4]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,16.5,lb,ub)
sd(s)
#0.5, 1, 5, 10
#0.8,1.6,8.2,16.5

# MHD ----------
post_param <- read_csv('./RIC/output/results/fit_pilot/MHD_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

## a ============
mu <- mu_post[1]; lb <- 0; ub <- 2
a <- rtruncnorm(n,mu,100,lb,ub)
sd(a)
#0.5, 1, 5, 10
#0.1,0.79,100,100

## c ================
mu <- mu_post[2]; lb <- 0; ub <- 1
c <- rtruncnorm(n,mu,100,lb,ub)
sd(c)
#0.5, 1, 5, 10
#0.1,100,100,100

## loghd ============
mu <- mu_post[3]; lb <- -1000000; ub <- 0
loghd <- rtruncnorm(n,mu,7.74,lb,ub)
sd(loghd)
#0.5, 1, 5, 10
#0.1,0.5,1.12,7.75

## loghr ============
mu <- mu_post[4]; lb <- 0; ub <- 1000000
loghr <- rtruncnorm(n,mu,8,lb,ub)
sd(loghr)
#0.5, 1, 5, 10
#0.1,0.57,1.39,8

## sd ============
mu <- mu_post[5]; lb <- 0; ub <- 1
sr <- rtruncnorm(n,mu,0.11,lb,ub)
sd(sr)
# 0.5, 1, 5, 10
# 0.1,100,100,100

## sr ==========
mu <- mu_post[6]; lb <- 0; ub <- 1
sd <- rtruncnorm(n,mu,0.1,lb,ub)
sd(sd)
# 0.5, 1, 5, 10

## s ==============
mu <- mu_post[7]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.2,lb,ub)
sd(s)
#0.1 0.5, 1, 5
#0.1,0.74,1.6,8.2

# PTT ----------
post_param <- read_csv('./RIC/output/results/fit_pilot/PTT_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post 

sd_post <- signif(post_param$sd,1)
sd_post

## alpha ========
mu <- mu_post[1]; lb <- 0; ub <- 1000000
alpha <- rtruncnorm(n,mu,0.81,lb,ub)
sd(alpha)
# 0.01,0.05,0.1,0.5
# 0.01,0.06,0.14,0.81

## beta ==========
mu <- mu_post[2]; lb <- 0; ub <- 1
beta <- rtruncnorm(n,mu,100,lb,ub)
sd(beta)
# 0.1, 0.5, 1, 5
# 0.1,100,100,100

## gamma ========
mu <- mu_post[3]; lb <- 0; ub <- 1
gamma <- rtruncnorm(n,mu,0.11,lb,ub)
sd(gamma)
# 0.1, 0.5,1,5
# 0.1,100,100,100

## R =========
mu <- mu_post[4]; lb <- 0; ub <- 1000000
R <- rtruncnorm(n,mu,7.92,lb,ub)
sd(R)
# 0.1, 0.5, 1, 5
# 0.1,0.52,1.3,7.93

## s =======
mu <- mu_post[5]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.08,lb,ub)
sd(s)
# 0.1, 0.5, 1, 5
# 0.1,0.61,1.44,8.08

# RITCH ------------
post_param <- read_csv('./RIC/output/results/fit_pilot/RITCH_postparam.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

## sig_beta_to ==========
mu <- 0; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.29,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.17,0.83, 1.66,8.29

## sig_beta_xt ==========
mu <- mu_post[1]; lb <- -1000000; ub <- 0
s <- rtruncnorm(n,mu,8.24,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.11,0.77, 1.6,8.24
## sig_beta_xp ==========
mu <- mu_post[2]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.29,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.17,0.83, 1.66,8.29

## mu_beta_xa =======
mu <- mu_post[3]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.083,lb,ub)
sd(s)
#0.001, 0.005, 0.01, 0.05
#0.0013,0.0079,0.016,0.083
## mu_beta_xr ====== 
mu <- mu_post[4]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.13,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.1,0.65,1.49,8.12

## mu_beta_pa ==========
mu <- mu_post[5]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.09,lb,ub)
sd(s)
# 0.1, 0.5, 1, 5
#0.1,0.63,1.46,8.1

## mu_beta_pr ======= 
mu <- mu_post[6]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.1,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.1,0.62,1.45,8.09

## mu_beta_ta ========
mu <- mu_post[7]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.82,lb,ub)
sd(s)
#0.01, 0.05, 0.1, 0.5
#0.01,0.072,0.16,0.83

## mu_beta_tr ========
mu <- mu_post[8]; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,8.23,lb,ub)
sd(s)
#0.1, 0.5, 1, 5
#0.11,0.76,1.59,8.23