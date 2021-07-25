source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)

prev_df <- 
  readRDS("./RIC/data/processed/prev_df2.rds")

risky_df <- prev_df%>%filter(t1==0,t2==0)
rd_df<-prev_df%>%filter((p1!=1|p2!=1),(t1!=0|t2!=0))

# Fit delay --------------
delay_df <-
  readRDS("./RIC/data/processed/prev_df2.rds")%>%
  filter(p1==1,p2==1)
range(delay_df$x1) #50-4000
range(delay_df$x2) #50.5-5000
range(delay_df$t1) #0-0.5
range(delay_df$t2) #0.25-60
range(delay_df$t1-delay_df$t2) #-60 ~ -0.25

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

parameters <- c('a','logh','i','s')
samples <- stan(file='./RIC/src/3_fit_prev/fit_HD.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 12)
saveRDS(samples,"./RIC/output/results/fit_prev/HD2.rds")

jpeg("./RIC/output/fig/fit_prev/HD2_trace.jpg")
traceplot(samples,pars=parameters)
dev.off()
jpeg("./RIC/output/fig/fit_prev/HD2_pair.jpg")
pairs(samples,pars=parameters)
dev.off()

# MHD ----------
parameters <- c('a','s','loghd','s_d','loghr','c','s_r')
samples <- stan(file='./RIC/src/3_fit_prev/fit_MHD.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 12)
saveRDS(samples,"./RIC/output/results/fit_prev/MHD2.rds")

jpeg("./RIC/output/fig/fit_prev/MHD2_trace.jpg")
traceplot(samples,pars=parameters)
dev.off()
jpeg("./RIC/output/fig/fit_prev/MHD2_pair.jpg")
pairs(samples,pars=parameters)
dev.off()

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
parameters <- c('alpha','beta','gamma','R','S','s')
samples <- stan(file='./RIC/src/3_fit_prev/fit_PTT.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,"./RIC/output/results/fit_prev/PTT2.rds")

jpeg("./RIC/output/fig/fit_prev/PTT2_trace.jpg")
traceplot(samples,pars=parameters)
dev.off()
jpeg("./RIC/output/fig/fit_prev/PTT2_pair.jpg")
pairs(samples,pars=parameters)
dev.off()

# RITCH ---------------
data<-list(
  nTrial=nrow(prev_df),
  n=prev_df$n,
  N=max(prev_df$n),
  #rva_ind=prev_df$p2<1,
  xs = sign(prev_df$x1 - prev_df$x2),
  ps = sign(prev_df$p1 - prev_df$p2),
  ts = sign(prev_df$t2 - prev_df$t1),
  xd = prev_df$x1 - prev_df$x2,
  xr = 2*(prev_df$x1 - prev_df$x2)/(prev_df$x1 + prev_df$x2),
  pd = prev_df$p1 - prev_df$p2,
  pr = 2*(prev_df$p1 - prev_df$p2)/(prev_df$p1 + prev_df$p2),
  td = prev_df$t2 - prev_df$t1,
  tr = 2*(prev_df$t2 - prev_df$t1)/(prev_df$t1 + prev_df$t2),
  k=prev_df$k
)
data$tr[is.na(data$tr)] <- 0
parameters <- c(#'beta_rva','beta_dva',
  'beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr')
samples <- stan(file='./RIC/src/3_fit_prev/fit_RITCH.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,"./RIC/output/results/fit_prev/RITCH2.rds")

jpeg("./RIC/output/fig/fit_prev/RITCH2_trace.jpg")
traceplot(samples,pars=parameters)
dev.off()
jpeg("./RIC/output/fig/fit_prev/RITCH2_pair.jpg")
pairs(samples,pars=parameters)
dev.off()
