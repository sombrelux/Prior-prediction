rm(list = ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 6)
library(data.table)
# Fit IM ---------
exp1_dt <- readRDS('./VWM/data/processed/IM_exp1.rds')
parameters <- c('a','b','r','s','kappa','delta',
                'xpred')

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
               refresh = 10,
               warmup=1000,
               chains=4, 
               cores=4,
               seed = 123)
ypred <- rstan::extract(fit_im)$xpred
fwrite(ypred, "./VWM/output/results/fit_prev/exp1.csv")

## post inference =============
parameters <- c('a','b','r','s','kappa','delta',
                'xpred')
png('./VWM/output/fig/fit_prev/pairs_im.png',width = 520, height = 520)
pairs(fit_im,pars = parameters[1:6])
dev.off()

png('./VWM/output/fig/fit_prev/trace_im.png')
traceplot(fit_im,pars = parameters[1:6])
dev.off()

post_param <- as.data.frame(summary(fit_im)$summary[1:6,])%>%
  rownames_to_column()
post_param
write_csv(post_param,
          './VWM/output/results/fit_prev/param_im.csv')

## k2sd ================
k2sd <- function(kappa) sqrt(-2*log(besselI(kappa,1,expon.scaled = T)/besselI(kappa,0,expon.scaled = T)))
k2sd(8.32)/pi*180
k2sd(9.97+8.32)/pi*180

# post prediction ----------------
library(bayestestR)
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

exp1_dt <- readRDS('./VWM/data/processed/IM_exp1.rds')
ypred <- fread('./VWM/output/results/fit_prev/exp1.csv')

ytarg <- exp1_dt$x
ypred_rad <- ypred/180*pi
ytarg_rad <- ytarg/180*pi

abs_err <- apply(ypred_rad,1,function(u) abs(wrap(u-ytarg_rad)))
dim(abs_err)
mae_err <- colMeans(abs_err)
mae_err_ci <- hdi(abs_err,ci=0.9)
mae_err_ci

