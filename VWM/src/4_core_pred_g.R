rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# prior prediction --------------
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')
post_param <- read_csv('./VWM/output/results/fit_prev/param_choice.csv')
post_param

mu_post <- signif(post_param$mean[1:6],2)
sig_post <- signif(post_param$sd[1:6],2)
parameters <- 'ypred'

i=1;a_w=1;b_w=2
data <- list(nPart=exp4_dt$nPart,
             ID = exp4_dt$ID,
             nTrial=length(exp4_dt$ID),
             N=exp4_dt$N,
             Condition = exp4_dt$Condition,
             M = exp4_dt$Setsize,
             m=exp4_dt$m,#orientations
             Dcol=exp4_dt$Dcol,#dist of col
             Dloc=exp4_dt$Dloc,#dist of loc
             X=exp4_dt$X, #360 candidate resp
             mu_a = mu_post[1], mu_b = mu_post[2], mu_r = mu_post[3],
             mu_sloc = mu_post[4], mu_scol = mu_post[4], 
             mu_kappa = mu_post[5], mu_delta = mu_post[6],
             a_w = a_w,
             sig_a = sig_post[1]*i, sig_b = sig_post[2]*i, sig_r = sig_post[3]*i,
             sig_sloc = sig_post[4]*i, sig_scol = sig_post[4]*i, 
             sig_kappa = sig_post[5]*i, sig_delta = sig_post[6]*i,
             b_w = b_w)

samples <- stan(
  file = './VWM/src/prior_IM.stan',
  data = data, 
  pars = parameters,
  iter = 1000,
  warmup = 0,
  chains = 20,
  cores = 20,
  algorithm="Fixed_param")
ypred <- extract(samples)$ypred
dim(ypred)

write.table(ypred,
            file = paste0("./VWM/output/results/prior_pred/IM_",
               i,"_",a_w,"_",b_w,".txt"),
            sep = ' ',
            row.names = FALSE)


# core prediction -------------
rm(list=ls())
library(HDInterval)

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')

ytarg <- exp4_dt$m[,1]

error_prior <- apply(ypred,2,function(u) wrap(u-ytarg))
err_k<-ypred[,1]
hist(err_k)

prior_pred <- list(
  ID = exp4_dt$ID,
  Orient = ,
  Condition = exp4_dt$Condition,
  Dloc = exp4_dt$Dloc,
  Dcol = exp4_dt$Dcol,
  ypred = ypred/180*pi,
  ytrue = exp4_dt$response
)

