source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)
Set <- 'GW'
prev_df<-
  read_csv("./RIC/data/processed/prev_df.csv")%>%
  filter(set==Set)

# Fit previous datasets -------------
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
Set <- 'SR'
prev_df<-
  read_csv("./RIC/data/processed/prev_df.csv")%>%
  filter(set==Set)
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

kpred <- data.frame(extract(samples)$kpred)
dim(kpred)

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/HD_',
                 Set,'_stats.csv'))

hdi_hd <- hdi(kpred,ci=0.99)%>%
  add_column(true=prev_df$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     prev_df)
write_csv(hdi_hd,
          paste0('./RIC/output/results/fit_prev/HD_',
                 Set,'_hdi.csv'))
## MHD ----------
samples <- 
  readRDS(paste0("./RIC/output/results/fit_prev/MHD_",
                 Set,".rds"))
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

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/MHD_',
                 Set,'_stats.csv'))

kpred <- data.frame(extract(samples)$kpred)
dim(kpred) 
hdi_mhd <- hdi(kpred,ci=0.99)%>%
  add_column(true=prev_df$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     prev_df)
write_csv(hdi_mhd,
          paste0('./RIC/output/results/fit_prev/MHD_',
                 Set,'_hdi.csv'))

## PTT --------
samples <- 
  readRDS(paste0("./RIC/output/results/fit_prev/PTT_",
                 Set,".rds"))
parameters <- c('alpha','beta','gamma','R','S','kpred')

jpeg(paste0("./RIC/output/fig/fit_prev/PTT_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters[1:5])
dev.off()
jpeg(paste0("./RIC/output/fig/fit_prev/PTT_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters[1:5])
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/PTT_',
                 Set,'_stats.csv'))

kpred <- data.frame(extract(samples)$kpred)
dim(kpred) 
hdi_ptt <- hdi(kpred,ci=0.99)%>%
  add_column(true=prev_df$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     prev_df)
write_csv(hdi_ptt,
          paste0('./RIC/output/results/fit_prev/PTT_',
                 Set,'_hdi.csv'))

## RITCH ---------------
samples <- 
  readRDS(paste0("./RIC/output/results/fit_prev/RITCH_",
                 Set,".rds"))
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

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 Set,'_stats.csv'))

kpred <- data.frame(extract(samples)$kpred)
dim(kpred) 
hdi_ritch <- hdi(kpred,ci=0.99)%>%
  add_column(true=prev_df$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     prev_df)
write_csv(hdi_ritch,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 Set,'_hdi.csv'))
