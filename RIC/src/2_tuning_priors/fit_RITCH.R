rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
#Sys.setenv(STAN_NUM_THREADS = 4)

# available data sets ------------
## money below 15000, delay below 6 years
retr_set <- read_csv("./RIC/data/previous/Retrieval.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

table(retr_set$Exp)

## delay =============
#for(i in retr_delay$Exp){
#i <- unique(retr_delay$Exp)[1]
#i  
#retr_temp <- retr_delay%>%filter(Exp==i)

retr_delay <- retr_set%>%filter(t2>0,p2==1) #delay
table(retr_delay$Exp)

Set_list <- unique(retr_delay$Exp)
Set_list
retr_delay$Exp_ind <- rep(0,length(retr_delay$Exp))
for(i in 1:length(Set_list)){
  retr_delay$Exp_ind[retr_delay$Exp==Set_list[i]] <- i
}

parameters <- c('beta_o','beta_xa','beta_xr',
                'beta_ta','beta_tr','sd_i',
				'beta_o_i','beta_xa_i','beta_xr_i',
                'beta_ta_i','beta_tr_i')

data<-list(
  nExp = length(Set_list),Exp = retr_delay$Exp_ind,
  nTrial=nrow(retr_delay),N = retr_delay$N,
  xd = retr_delay$xd,td = retr_delay$td,
  xr = retr_delay$xr,tr = retr_delay$tr,
  y = retr_delay$y)

samples <- stan(file = './RIC/src/2_tuning_priors/fit_RITCH_delay.stan',
                data = data,
                pars = parameters,
                iter = 2000,
                warmup = 1000,
                chains = 4, 
                thin = 4,
                cores = 4,
                seed = 123,
                verbose = TRUE,
                refresh = 50,
                control = list(max_treedepth = 15))
saveRDS(samples, 
        './RIC/output/results/fit_prev/RITCH_delay.rds')

## risky ==================
retr_risky <- retr_set%>%filter(t2==0,p2<1) 
table(retr_risky$Exp)

parameters <- c('beta_o','beta_xa','beta_xr',
                'beta_pa','beta_pr','sd_i',
                'beta_o_i','beta_xa_i','beta_xr_i',
                'beta_pa_i','beta_pr_i')

Set_list <- unique(retr_risky$Exp)
Set_list
retr_risky$Exp_ind <- rep(0,length(retr_risky$Exp))
for(i in 1:length(Set_list)){
  retr_risky$Exp_ind[retr_risky$Exp==Set_list[i]] <- i
}

#for(i in retr_risky$Exp){
#i <- unique(retr_risky$Exp)[1]
#i  
#retr_temp <- retr_risky%>%filter(Exp==i)
data<-list(
  nExp = length(Set_list),Exp = retr_risky$Exp_ind,
  nTrial=nrow(retr_risky), N = retr_risky$N,
  xd = retr_risky$xd,pd = retr_risky$pd,
  xr = retr_risky$xr,pr = retr_risky$pr,
  y = retr_risky$y)

samples <- stan(file = './RIC/src/2_tuning_priors/fit_RITCH_risky.stan',
                data = data,
                pars = parameters,
                iter = 2000,
                warmup = 1000,
                chains = 4, 
                thin = 4,
                cores = 4,
                seed = 123,
                verbose = TRUE,
                refresh = 50,
                control = list(max_treedepth = 15))
saveRDS(samples, 
        './RIC/output/results/fit_prev/RITCH_risky.rds')

# RIC: +indifference points ---------------------

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_delay.csv'))

png(paste0('./RIC/output/fig/fit_prev/Pairs_RITCH_delay.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()

png(paste0('./RIC/output/fig/fit_prev/Trace_RITCH_',i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
traceplot(samples, pars = parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_',i,'.csv'))

png(paste0('./RIC/output/fig/fit_prev/Pairs_RITCH_',i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()

png(paste0('./RIC/output/fig/fit_prev/Trace_RITCH_',i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
traceplot(samples, pars = parameters)
dev.off()
# Indifference point -------------------
indiff_set <- read_csv("./RIC/data/previous/Indiff.csv")%>%
  add_column(Paradigm='Indiff',t1=0,p1=1)%>%
  mutate(y=round(N/2))%>%
  rename(x1=Indifferences,x2=Amounts,t2=Delay,p2=Probability)%>%
  dplyr::select(Paradigm,Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))
table(indiff_set$Exp)

parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr')

## RIC ================
indiff_RI <- indiff_set%>%filter(t2>0,p2<1)
unique(indiff_RI$Exp)
i <- unique(indiff_RI$Exp)[3]
i  

indiff_temp <- indiff_set%>%filter(Exp==i)
data<-list(
    nTrial=nrow(indiff_temp),N = indiff_temp$N,
    xs = indiff_temp$xs,ts = indiff_temp$ts,ps = indiff_temp$ps,
    xd = indiff_temp$xd,td = indiff_temp$td,pd = indiff_temp$pd,
    xr = indiff_temp$xr,tr = indiff_temp$tr,pr = indiff_temp$pr,
    y = indiff_temp$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_1.stan',
                  data=data,
                  pars=parameters,
                  iter = 8000,
                  warmup = 4000,
                  chains = 4, 
                  thin = 4,
                  cores = 4,
                  seed = 123,
                  verbose = TRUE,
                  refresh = 100,
                  control = list(max_treedepth = 15))
saveRDS(samples,paste0('./RIC/output/results/fit_prev/RITCH_indiff_',i,'.rds'))


png(paste0('./RIC/output/fig/fit_prev/RITCH_indiff_pairs_',i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()

png(paste0('./RIC/output/fig/fit_prev/RITCH_indiff_trace_',i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
traceplot(samples, pars = parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_indiff_',i,'.csv'))

## Delay ==============

## Risky ==============
