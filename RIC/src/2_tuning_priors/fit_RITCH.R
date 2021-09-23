rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# available data sets ------------
## money below 15000, delay below 6 years
retr_set <- read_csv("./RIC/data/previous/Retrieval.csv")%>%
  dplyr::select(Paradigm,Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

retr_set$Exp_ind <- rep(0,length(retr_set$Exp))
Set_list <- unique(retr_set$Exp)
for(i in 1:length(Set_list)){
  ind <- retr_set$Exp==Set_list[i]
  retr_set$Exp_ind[ind] <- i
}
parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'SD_i')

data<-list(
  nTrial=nrow(retr_set),nExp=length(Set_list),
  N = retr_set$N, Exp = retr_set$Exp_ind,
  xs = retr_set$xs,ts = retr_set$ts,ps = retr_set$ps,
  xd = retr_set$xd,td = retr_set$td,pd = retr_set$pd,
  xr = retr_set$xr,tr = retr_set$tr,pr = retr_set$pr,
  y = retr_set$y)

samples <- stan(file = './RIC/src/2_tuning_priors/fit_RITCH_1.stan',
                data = data,
                pars = parameters,
                iter = 8000,
                warmup = 4000,
                chains = 4, 
                thin = 4,
                cores = 4,
                seed = 123,
                verbose = TRUE,
                refresh = 50,
                control = list(max_treedepth = 15))
saveRDS(samples, './RIC/output/results/fit_prev/RITCH_retr.rds')
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_retr_stats.csv')

## posterior ==================
png("./RIC/output/fig/fit_prev/RITCH_retr_pairs.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()

png("./RIC/output/fig/fit_prev/RITCH_retr_trace.png",
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

indiff_set$Exp_ind <- rep(0,length(indiff_set$Exp))
Set_list <- unique(indiff_set$Exp)
for(i in 1:length(Set_list)){
  ind <- indiff_set$Exp==Set_list[i]
  indiff_set$Exp_ind[ind] <- i
}
parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'SD_i')

data<-list(
	nTrial=nrow(indiff_set),nExp=length(Set_list),
	N = indiff_set$N, Exp = indiff_set$Exp_ind,
	xs = indiff_set$xs,ts = indiff_set$ts,ps = indiff_set$ps,
	xd = indiff_set$xd,td = indiff_set$td,pd = indiff_set$pd,
	xr = indiff_set$xr,tr = indiff_set$tr,pr = indiff_set$pr,
	y = indiff_set$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_2.stan',
                  data=data,
                  pars=parameters,
                  iter=8000,
                  warmup = 4000,
                  chains=4, 
                  thin=4,
                  cores=4,
                  seed = 123,
                verbose = TRUE,
                refresh = 50,
                control = list(max_treedepth = 15))
saveRDS(samples,
        './RIC/output/results/fit_prev/RITCH_indiff.rds')
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_indiff_stats.csv')
## posterior ==================
png("./RIC/output/fig/fit_prev/RITCH_indiff_pairs.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()

png("./RIC/output/fig/fit_prev/RITCH_indiff_trace.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
traceplot(samples, pars = parameters)
dev.off()
