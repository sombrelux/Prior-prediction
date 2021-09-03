rm(list = ls())
library(tidyverse)
library(ggpubr)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# Fit individual ---------
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')
pw <- './VWM/output/results/fit_observation'
parameters <- c('a','b','r','sloc','scol','w',
                'kappa','delta')
for(i in 1:exp4_dt$nPart){
  ind_i <- exp4_dt$ID==i
  data_i <- list(
    nTrial = sum(ind_i),
    N = exp4_dt$N,
    M = exp4_dt$Setsize,
    Condition = exp4_dt$Condition[ind_i],
    Dloc = exp4_dt$Dloc[ind_i,],
    Dcol = exp4_dt$Dcol[ind_i,],
    E = exp4_dt$E[ind_i,,], 
    x = exp4_dt$response[ind_i]
  )
  
  fit_im <- stan(file='./VWM/src/fit_im_exp4.stan',
                 data=data_i,
                 pars=parameters,
                 iter=1500,
                 warmup=1000,
                 chains=4, 
                 cores=4,
                 seed = 123)
  saveRDS(fit_im,
          paste0(pw,'/subj_',i,'.rds'))
  rm(list = c('ind_i','data_i','fit_im'))
}