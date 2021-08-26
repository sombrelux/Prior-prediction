rm(list = ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# Fit individual ---------
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
pw <- './VWM/output/results/fit_previous'
parameters <- c('a','b','r','s',
                'kappa','delta')
for(i in 1:exp1_dt$nPart){
  ind_i <- exp1_dt$ID==i
  data_i <- list(
      nTrial = sum(ind_i),
      N = exp1_dt$N, M = exp1_dt$M,
      Setsize = exp1_dt$Setsize[ind_i],
      ind_mat = exp1_dt$ind_mat[ind_i,], 
      D = exp1_dt$D[ind_i,], 
      E = exp1_dt$E[ind_i,,], 
      x = exp1_dt$x[ind_i]
  )
    
  fit_im <- stan(file='./VWM/src/fit_im_exp1.stan',
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

# pooled all individuals -----------
rm(list = ls())

exp1_dt <- readRDS('./VWM/data/processed/exp1_dt.rds')
s <- 20
pw <- paste0('./VWM/output/results/fit_prev/s=',s)
m <- Setsize <- D <- response <- ypred_pool <- NULL
for(i in exp1_dt$subjID){
  ind_i <- exp1_dt$subjects==i
  m <- rbind(m, exp1_dt$m[ind_i,])
  Setsize <- c(Setsize,exp1_dt$Setsize[ind_i])
  D <- rbind(D, exp1_dt$Dist[ind_i,])
  response <- c(response, exp1_dt$response[ind_i])
  
  samples <- readRDS(paste0(pw,"/subj_",i,".rds"))
  ypred <- t(rstan::extract(samples)$xpred)
  ypred_rad <- apply(ypred,2,function(u) exp1_dt$X[u])
  ypred_pool <- rbind(ypred_pool,ypred_rad)
}
fit_pool <- list(m=m,D=D,
                 Setsize=Setsize,
                 response=response,
                 ypred=ypred_pool)
saveRDS(fit_pool,
        paste0('./VWM/output/results/fit_prev/pool_results_',
               s,'.rds'))

# plots ---------------
rm(list = ls())
exp1_dt <- readRDS('./VWM/data/processed/exp1_dt.rds')

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
s <- 20
pw <- paste0('./VWM/output/results/fit_prev/s=',s)
pw2 <- paste0('./VWM/output/fig/fit_prev/s=',s)
if(!dir.exists(pw2)) dir.create(pw2)
fit_pool <- readRDS(paste0(pw,'/pool_results.rds'))

source('./VWM/src/3_fit_prev/post_plots.R')
