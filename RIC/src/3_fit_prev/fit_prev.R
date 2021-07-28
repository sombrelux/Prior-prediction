source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)
Set <- 'SR'
prev_df<-
  read_csv("./RIC/data/processed/prev_df.csv")%>%
  filter(set==Set)

# Fit prev_df -------------
## HD ----------
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
  k=round(prev_df$k))

parameters <- c('a','logh','i','s','kpred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_HD.stan',
                   data=data,
                   pars=parameters,
                   chains=4, 
                   thin=4,
                   cores=4,
                   seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/fit_prev/HD_",
               Set,".rds"))

## MHD ===============
rm(list = c('data','samples','parameters'))

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
  k=round(prev_df$k))
parameters <- c('a','c','s',
                'loghd','loghr',
                's_d','s_r',
                'kpred')

samples <- stan(file='./RIC/src/3_fit_prev/fit_MHD.stan',
                data=data,
                pars=parameters,
                iter=4000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                control = list(adapt_delta = 0.99))
saveRDS(samples,
        paste0("./RIC/output/results/fit_prev/MHD_",
               Set,".rds"))
## PTT ================
rm(list = c('data','samples','parameters'))
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
  k=round(prev_df$k))
parameters <- c('alpha','beta','gamma','R','S','kpred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_PTT.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/fit_prev/PTT_",
               Set,".rds"))

## RITCH =============
rm(list = c('data','samples','parameters'))
data<-list(
  nTrial=nrow(prev_df),
  n=prev_df$n,
  N=max(prev_df$n),
  rva_ind=prev_df$p2<1,
  xd = prev_df$x1 - prev_df$x2,
  xr = 2*(prev_df$x1 - prev_df$x2)/(prev_df$x1 + prev_df$x2),
  pd = prev_df$p1 - prev_df$p2,
  pr = 2*(prev_df$p1 - prev_df$p2)/(prev_df$p1 + prev_df$p2),
  td = prev_df$t2 - prev_df$t1,
  tr = 2*(prev_df$t2 - prev_df$t1)/(prev_df$t1 + prev_df$t2),
  k=round(prev_df$k)
)
data$tr[is.na(data$tr)] <- 0
parameters <- c('beta_rva','beta_dva',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr','kpred')

samples <- stan(file='./RIC/src/3_fit_prev/fit_RITCH_p2.stan',
                data=data,
                pars=parameters,
                iter=4000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/fit_prev/RITCH_",
               Set,".rds"))

# Check -------------
rm(list=ls())
## HD ========
samples <- 
  readRDS(paste0("./RIC/output/results/fit_prev/HD_",
                          Set,".rds"))
parameters <- c('a','logh','i','s','kpred')

jpeg(paste0("./RIC/output/fig/fit_prev/HD_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters[1:4])
dev.off()
jpeg(paste0("./RIC/output/fig/fit_prev/HD_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters[1:4])
dev.off()

kpred <- extract(samples)$kpred
dim(kpred) 
prop.1.opt <- data.frame(t(apply(kpred,1,
                                 function(u) u/data$n)))
hdi_hd <- hdi(prop.1.opt,ci=0.99)%>%
  add_column(true=data$k/data$n)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))
bad_fit <- cbind(hdi_hd%>%filter(check==F),
                 prev_df[!hdi_hd$check,])
write_csv(bad_fit,
          paste0('./RIC/output/results/fit_prev/HD_',
                 Set,'bad_fit.csv'))
## MHD ----------
rm(list=ls())
samples <- 
  readRDS("./RIC/output/results/fit_prev/MHD.rds")
parameters <- c('a','c','s',
                'loghd','loghr',
                's_d','s_r',
                'kpred')

jpeg(paste0("./RIC/output/fig/fit_prev/MHD_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters[1:7])
dev.off()
jpeg(paste0("./RIC/output/fig/fit_prev/MHD_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters[1:7])
dev.off()

kpred <- extract(samples)$kpred
dim(kpred) 
prop.1.opt <- data.frame(t(apply(kpred,1,
                                 function(u) u/data$n)))
hdi_mhd <- hdi(prop.1.opt,ci=0.99)%>%
  add_column(true=data$k/data$n)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))
bad_fit <- cbind(hdi_mhd%>%filter(check==F),
                 prev_df[!hdi_hd$check,])
write_csv(bad_fit,
          paste0('./RIC/output/results/fit_prev/MHD_',
                 Set,'bad_fit.csv'))

## PTT --------
rm(list=ls())
samples <- 
  readRDS("./RIC/output/results/fit_prev/PTT.rds")
parameters <- c('alpha','beta','gamma','R','S','kpred')

jpeg(paste0("./RIC/output/fig/fit_prev/PTT_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters[1:5])
dev.off()
jpeg(paste0("./RIC/output/fig/fit_prev/PTT_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters[1:5])
dev.off()

kpred <- extract(samples)$kpred
dim(kpred) 
prop.1.opt <- data.frame(t(apply(kpred,1,
                                 function(u) u/data$n)))
hdi_ptt <- hdi(prop.1.opt,ci=0.99)%>%
  add_column(true=data$k/data$n)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))
bad_fit <- cbind(hdi_ptt%>%filter(check==F),
                 prev_df[!hdi_hd$check,])
write_csv(bad_fit,
          paste0('./RIC/output/results/fit_prev/PTT_',
                 Set,'bad_fit.csv'))

## RITCH ---------------
rm(list=ls())
samples <- 
  readRDS("./RIC/output/results/fit_prev/RITCH.rds")
parameters <- c('beta_rva','beta_dva',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr','kpred')

jpeg(paste0("./RIC/output/fig/fit_prev/RITCH_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters[1:8])
dev.off()
jpeg(paste0("./RIC/output/fig/fit_prev/RITCH_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters[1:8])
dev.off()

kpred <- extract(samples)$kpred
dim(kpred) 
prop.1.opt <- data.frame(t(apply(kpred,1,
                                 function(u) u/data$n)))
hdi_ritch <- hdi(prop.1.opt,ci=0.99)%>%
  add_column(true=data$k/data$n)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))
bad_fit <- cbind(hdi_ritch%>%filter(check==F),
                 prev_df[!hdi_hd$check,])
write_csv(bad_fit,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 Set,'bad_fit.csv'))