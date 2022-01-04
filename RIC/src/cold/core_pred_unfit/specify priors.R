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
post_param <- read_csv('./RIC/output/results/fit_prev/HD_param_choice.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

## a ==========
mu <- 0.2; lb <- 0; ub <- 2
a <- rtruncnorm(n,mu,0.05,lb,ub)
sd(a)
#0.001, 0.005, 0.01, 0.05

## logh =======
mu <- -1.8; lb <- -1000000; ub <- 0
logh <- rtruncnorm(n,mu,0.5,lb,ub)
sd(logh)
#0.01, 0.05, 0.1, 0.5

## i ===============
mu <- 2.4; lb <- 0; ub <- 1000000
i <- rtruncnorm(n,mu,7.6,lb,ub)
sd(i)
#0.1, 0.5, 1, 5
#0.1,0.5,1,7.6

## s ===============
mu <- 2.2; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.1,lb,ub)
sd(s)
#0.01, 0.05, 0.1, 0.5

# MHD ----------
post_param <- read_csv('./RIC/output/results/fit_prev/MHD_param_choice.csv')
mu_post <- signif(post_param$mean,2)
mu_post # c peak at 0

sd_post <- signif(post_param$sd,1)
sd_post

## a ============
mu <- 0.11; lb <- 0; ub <- 2
a <- rtruncnorm(n,mu,0.05,lb,ub)
sd(a)
#0.001, 0.005, 0.01, 0.05
#0.001, 0.005, 0.01,0.053
## c ================
mu <- 0; lb <- 0; ub <- 1
c <- rtruncnorm(n,mu,0.083,lb,ub)
sd(c)
#0.001, 0.005, 0.01, 0.05
#0.0017,0.0083,0.017,0.083
## loghd ============
mu <- -0.13; lb <- -1000000; ub <- 0
loghd <- rtruncnorm(n,mu,0.79,lb,ub)
sd(loghd)
#0.01, 0.05, 0.1, 0.5
#0.01, 0.051, 0.12, 0.79
## loghr ============
mu <- 1.6; lb <- 0; ub <- 1000000
loghr <- rtruncnorm(n,mu,7.8,lb,ub)
sd(loghr)

#0.1, 0.5, 1, 5
#0.1,0.5,1.16,7.8

## sr ============
mu <- 0.13; lb <- 0; ub <- 1
sr <- rtruncnorm(n,mu,0.052,lb,ub)
sd(sr)
# 0.001, 0.005, 0.01, 0.05
# 0.001, 0.005, 0.01, 0.051

## sd ==========
mu <- 0.089; lb <- 0; ub <- 1
sd <- rtruncnorm(n,mu,0.056,lb,ub)
sd(sd)
# 0.001, 0.005, 0.01, 0.05
# 0.001, 0.005, 0.01, 0.056

## s ==============
mu <- 10; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,5.5,lb,ub)
sd(s)
#0.1 0.5, 1, 5
#0.1 0.5, 1, 5.4

# PTT ----------
post_param <- read_csv('./RIC/output/results/fit_prev/PTT_param_choice.csv')
mu_post <- signif(post_param$mean,2)
mu_post 

sd_post <- signif(post_param$sd,1)
sd_post

## alpha ========
mu <- 0.1; lb <- 0; ub <- 1000000
alpha <- rtruncnorm(n,mu,0.054,lb,ub)
sd(alpha)
# 0.001,0.005,0.01,0.05
# 0.001,0.005,0.01,0.054

## beta ==========
mu <- 0.39; lb <- 0; ub <- 1
beta <- rtruncnorm(n,mu,0.8,lb,ub)
sd(beta)
# 0.01, 0.05, 0.1, 0.5
# 0.01,0.05,0.1,100

## gamma ========
mu <- 0.83; lb <- 0; ub <- 1
gamma <- rtruncnorm(n,mu,0.11,lb,ub)
sd(gamma)
# 0.01, 0.05, 0.1, 0.5
# 0.01,0.05,0.11,100

## R =========
mu <- 4.7; lb <- 0; ub <- 1000000
R <- rtruncnorm(n,mu,6.8,lb,ub)
sd(R)
# 0.1, 0.5, 1, 5
# 0.1,0.5,1,6.8

## s =======
mu <- 1.1; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.52,lb,ub)
sd(s)
# 0.01, 0.05,0.1, 0.5
# 0.01,0.05,0.1,0.53

# RITCH ------------
post_param <- read_csv('./RIC/output/results/fit_prev/RITCH_param_choice.csv')
mu_post <- signif(post_param$mean,2)
mu_post

sd_post <- signif(post_param$sd,1)
sd_post

## sig_beta_xo ==========
mu <- 0; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,1.65,lb,ub)
sd(s)
#0.01, 0.05, 0.1, 0.5, 1
#0.017,0.083, 0.17,0.83, 1.66

## mu_beta_xt =============
mu <- 0.17; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.77,lb,ub)
sd(s)
#0.01, 0.05, 0.1, 0.5
#0.01,0.05,0.11,0.78
## mu_beta_xp ======
mu <- 0.79; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.584,lb,ub)
sd(s)
# 0.01, 0.05, 0.1, 0.5
#0.01, 0.05, 0.1,0.58

## mu_beta_xa =======
mu <- 0.00088; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.00057,lb,ub)
sd(s)
# 0.00001, 0.00005, 0.0001, 0.0005
#0.00001,0.00005,0.0001,0.00056

## mu_beta_xr ====== 
mu <- 2.1; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.5,lb,ub)
sd(s)
#0.01, 0.05, 0.1, 0.5

## mu_beta_pa ==========
mu <- 1.7; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,7.78,lb,ub)
sd(s)
# 0.1, 0.5, 1, 5
#0.1,0.5,1.14,7.77

## mu_beta_pr ======= 
mu <- 1.6; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.504,lb,ub)
sd(s)
#0.01, 0.05, 0.1, 0.5

## mu_beta_ta ========
mu <- 0.18; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.051,lb,ub)
sd(s)
# 0.001, 0.005, 0.01, 0.05

## mu_beta_tr ========
mu <- 0.4; lb <- 0; ub <- 1000000
s <- rtruncnorm(n,mu,0.704,lb,ub)
sd(s)
# 0.01, 0.05, 0.1, 0.5
#0.01,0.05,0.1,0.7