source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)

prev_df<-
  readRDS("./RIC/data/processed/prev_df.rds")

# HD ----------
data<-list(
  nTrial=nrow(prev_df),
  n=prev_df$n,
  N=max(prev_df$n),
  x1=prev_df$x1,
  x2=prev_df$x2,
  t1=prev_df$t1,
  t2=prev_df$t2,
  o1=1/prev_df$p1-1,
  o2=1/prev_df$p2-1,
  k=prev_df$k)

parameters <- c('a','h','i','s')
samples <- stan(file='./RIC/src/fit_HD.stan',
                   data=data,
                   pars=parameters,
                   chains=4, 
                   thin=4,
                   cores=4,
                   seed = 123)
saveRDS(samples,"./RIC/output/results/fit_prev/HD.rds")
traceplot(samples,pars=parameters)
pairs(samples,pars=parameters)

# MHD ----------
parameters <- c('a','s','hd','s_d')#'hr',,'c','s_r')
samples <- stan(file='./RIC/src/fit_MHD.stan',
                   data=data,
                   pars=parameters,
                iter=10,
                   chains=1, 
                   thin=1,
                   cores=1,
                   seed = 123)
saveRDS(samples,"./RIC/output/results/fit_prev/MHD.rds")
traceplot(samples,pars=parameters)
pairs(samples,pars=parameters)

# PTT --------

data<-list(
  nTrial=nrow(prev_df),
  n=prev_df$n,
  N=max(prev_df$n),
  x1=prev_df$x1,
  x2=prev_df$x2,
  t1=prev_df$t1,
  t2=prev_df$t2,
  p1=prev_df$p1,
  p2=prev_df$p2,
  k=prev_df$k)
parameters <- c('alpha','beta','gamma','R','s')
samples <- stan(file='./RIC/src/fit_PTT.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,"./RIC/output/results/fit_prev/PTT.rds")
traceplot(samples,pars=parameters)
pairs(samples,pars=parameters)

