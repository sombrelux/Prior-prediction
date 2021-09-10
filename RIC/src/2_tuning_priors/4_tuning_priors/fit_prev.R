source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)

prev_dt <- read_csv("./RIC/data/processed/df_temp.csv")
indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")
  
# fit indifference points -----------
Set_list <- prev_dt$set




# Fit previous datasets -------------
## HD ----------
data<-list(
  nTrial=nrow(df_temp),
  n=df_temp$n,
  N=max(df_temp$n),
  x1=df_temp$x1,
  x2=df_temp$x2,
  t1=df_temp$t1,
  t2=df_temp$t2,
  o1=1/df_temp$p1-1,
  o2=1/df_temp$p2-1,
  k=round(df_temp$k))

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
  nTrial=nrow(df_temp),
  n=df_temp$n,
  N=max(df_temp$n),
  x1=df_temp$x1,
  x2=df_temp$x2,
  t1=df_temp$t1,
  t2=df_temp$t2,
  o1=1/df_temp$p1-1,
  o2=1/df_temp$p2-1,
  k=round(df_temp$k))
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
  nTrial=nrow(df_temp),
  n=df_temp$n,
  N=max(df_temp$n),
  x1=df_temp$x1,
  x2=df_temp$x2,
  t1=df_temp$t1,
  t2=df_temp$t2,
  p1=df_temp$p1,
  p2=df_temp$p2,
  k=round(df_temp$k))
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





# Check -------------
Set <- 'SR'
df_temp<-
  read_csv("./RIC/data/processed/df_temp.csv")%>%
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
  add_column(true=df_temp$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     df_temp)
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
  add_column(true=df_temp$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     df_temp)
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
  add_column(true=df_temp$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     df_temp)
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
  add_column(true=df_temp$k)%>%
  mutate(check = (true>=CI_low)&(true<=CI_high))%>%
  bind_cols(     df_temp)
write_csv(hdi_ritch,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 Set,'_hdi.csv'))
