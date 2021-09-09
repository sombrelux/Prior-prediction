rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# prior prediction --------------
pw <- "./VWM/output/results/prior_prediction/"
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')

prior_file <- 'prior_narrow_pool'
dir.create(paste0(pw,prior_file))
parameters <- c('ypred')
data <- list(nPart=exp4_dt$nPart,
             ID = exp4_dt$ID,
             nTrial=length(exp4_dt$ID),
             N=exp4_dt$N,
             Condition = exp4_dt$Condition,
             M = exp4_dt$Setsize,
             m=exp4_dt$m,#orientations
             Dcol=exp4_dt$Dcol,#dist of col
             Dloc=exp4_dt$Dloc,#dist of loc
             X=exp4_dt$X #360 candidate resp
)
samples <- stan(
  file = paste0('./VWM/src/',prior_file,'.stan'),
  data = data, pars = parameters,
  iter = 2000,
  warmup = 0,
  chains = 4,
  cores = 4,
  algorithm="Fixed_param")

ypred <- t(rstan::extract(samples)$ypred)
dim(ypred)

prior_pred <- list(
  ID = exp4_dt$ID,
  Orient = exp4_dt$m,
  Condition = exp4_dt$Condition,
  Dloc = exp4_dt$Dloc,
  Dcol = exp4_dt$Dcol,
  ypred = ypred/180*pi,
  ytrue = exp4_dt$response
)
saveRDS(prior_pred,paste0(pw,prior_file,
                          "/prior_pred.rds"))

# core prediction -------------
rm(list=ls())

pw <- "./VWM/output/results/prior_prediction/prior_narrow_pool"
prior_pred <- readRDS(paste0(pw,"/prior_pred.rds"))
source('./VWM/src/core_pred.R')