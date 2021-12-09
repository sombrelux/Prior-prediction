rm(list = ls())
library(tidyverse)
library(ggpubr)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# Fit group ---------
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
parameters <- c('a','b','r','s',
                'kappa','delta')

data <- list(
  nTrial = nrow(exp1_dt),
  N = exp1_dt$N, M = exp1_dt$M,
  Setsize = exp1_dt$Setsize,
  ind_mat = exp1_dt$ind_mat, 
  D = exp1_dt$D, 
  E = exp1_dt$E, 
  x = exp1_dt$x
)

fit_im <- stan(file='./VWM/src/fit_im_exp1.stan',
               data=data,
               pars=parameters,
               iter=2000,
               warmup=1000,
               chains=4, 
               cores=4,
               seed = 123)
saveRDS(fit_im,
        './VWM/output/results/fit_previous/exp1.rds')
