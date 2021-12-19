rm(list = ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 6)

# Fit IM ---------
exp1_dt <- readRDS('./VWM/data/processed/IM_exp1.rds')
parameters <- c('a','b','r','s',
                'kappa','delta')

data <- list(
  nTrial = exp1_dt$nTrial,
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
               refresh = 50,
               warmup=1000,
               chains=4, 
               cores=4,
               seed = 123)
saveRDS(fit_im,
        './VWM/output/results/fit_prev/exp1_im.rds')

## post inference =============
fit_im <- readRDS('./VWM/output/results/fit_prev/exp1.rds')

png('./VWM/output/fig/fit_prev/pairs_im.png')
pairs(fit_im,pars = parameters)
dev.off()

png('./VWM/output/fig/fit_prev/trace_im.png')
traceplot(fit_im,pars = parameters)
dev.off()

post_param <- as.data.frame(summary(fit_im)$summary)%>%
  rownames_to_column()
post_param
write_csv(post_param,
          './VWM/output/results/fit_prev/param_im.csv')

# Fit Slots ---------
rm(list = ls())

exp1_dt <- readRDS('./VWM/data/processed/slot_exp1.rds')
parameters <- c('K','sigma1')

data <- list(
  nTrial = exp1_dt$nTrial,
  Setsize = exp1_dt$Setsize,
  m = exp1_dt$m,
  x = exp1_dt$x
)

fit_slot <- stan(file='./VWM/src/fit_slot_exp1.stan',
               data=data,
               pars=parameters,
               iter=2000,
               refresh = 50,
               warmup=1000,
               chains=4, 
               cores=4,
               seed = 123)
saveRDS(fit_slot,
        './VWM/output/results/fit_prev/exp1_slot.rds')


## post inference =============
fit_slot <- readRDS('./VWM/output/results/fit_prev/exp1_slot.rds')

png('./VWM/output/fig/fit_prev/pairs_slot.png')
pairs(fit_slot,pars = parameters)
dev.off()

png('./VWM/output/fig/fit_prev/trace_slot.png')
traceplot(fit_slot,pars = parameters)
dev.off()

post_param <- as.data.frame(summary(fit_slot)$summary)%>%
  rownames_to_column()
post_param
write_csv(post_param,
          './VWM/output/results/fit_prev/param_slot.csv')


