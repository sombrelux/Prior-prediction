rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
#Sys.setenv(STAN_NUM_THREADS = 4)

# choice sets ------------
## money below 15000, delay below 6 years
choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

table(choice_set$Exp)

## delay =============
#for(i in choice_delay$Exp){
#i <- unique(choice_delay$Exp)[1]
#i  
#choice_temp <- choice_delay%>%filter(Exp==i)

choice_delay <- choice_set%>%filter(t2>0,p2==1) #delay
table(choice_delay$Exp)

Set_list <- unique(choice_delay$Exp)
Set_list
choice_delay$Exp_ind <- rep(0,length(choice_delay$Exp))
for(i in 1:length(Set_list)){
  choice_delay$Exp_ind[choice_delay$Exp==Set_list[i]] <- i
}

parameters <- c('beta_o','beta_xa','beta_xr',
                'beta_ta','beta_tr','sd_i',
                'beta_o_i','beta_xa_i','beta_xr_i',
                'beta_ta_i','beta_tr_i')

data<-list(
  nExp = length(Set_list),Exp = choice_delay$Exp_ind,
  nTrial=nrow(choice_delay),N = choice_delay$N,
  xd = choice_delay$xd,td = choice_delay$td,
  xr = choice_delay$xr,tr = choice_delay$tr,
  y = choice_delay$y)

samples <- stan(file = './RIC/src/2_tuning_priors/fit_RITCH_delay.stan',
                data = data,
                pars = parameters,
                iter = 4000,
                warmup = 2000,
                chains = 4, 
                thin = 4,
                cores = 4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
saveRDS(samples, 
        './RIC/output/results/fit_prev/RITCH_delay.rds')

### posterior ==============
samples <- readRDS('./RIC/output/results/fit_prev/RITCH_delay.rds')
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_delay.csv')

png('./RIC/output/fig/fit_prev/Pairs_RITCH_delay.png',
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters[1:6])
dev.off()

for(i in 1:length(Set_list)){
  png(paste0('./RIC/output/fig/fit_prev/Pairs_RITCH_delay_',
             Set_list[i],'.png'),
      width = 6, height = 6, units = 'in', res = 300)
  par(mar=c(1,1,1,1))
  pairs(samples,pars = paste0(parameters[1:5],'_i[',i,']'))
  dev.off()
}

## risky ==================
choice_risky <- choice_set%>%filter(t2==0,p2<1) 
table(choice_risky$Exp)

parameters <- c('beta_o','beta_xa','beta_xr',
                'beta_pa','beta_pr','sd_i',
                'beta_o_i','beta_xa_i','beta_xr_i',
                'beta_pa_i','beta_pr_i')

Set_list <- unique(choice_risky$Exp)
Set_list
choice_risky$Exp_ind <- rep(0,length(choice_risky$Exp))
for(i in 1:length(Set_list)){
  choice_risky$Exp_ind[choice_risky$Exp==Set_list[i]] <- i
}

#for(i in choice_risky$Exp){
#i <- unique(choice_risky$Exp)[1]
#i  
#choice_temp <- choice_risky%>%filter(Exp==i)
data<-list(
  nExp = length(Set_list),Exp = choice_risky$Exp_ind,
  nTrial=nrow(choice_risky), N = choice_risky$N,
  xd = choice_risky$xd,pd = choice_risky$pd,
  xr = choice_risky$xr,pr = choice_risky$pr,
  y = choice_risky$y)

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
                refresh = 100,
                control = list(max_treedepth = 15))
saveRDS(samples, 
        './RIC/output/results/fit_prev/RITCH_risky.rds')

### posterior ==============
samples <- readRDS('./RIC/output/results/fit_prev/RITCH_risky.rds')
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_risky.csv')

png('./RIC/output/fig/fit_prev/Pairs_RITCH_risky.png',
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters[1:6])
dev.off()

for(i in 1:length(Set_list)){
  png(paste0('./RIC/output/fig/fit_prev/Pairs_RITCH_risky_',
             Set_list[i],'.png'),
      width = 6, height = 6, units = 'in', res = 300)
  par(mar=c(1,1,1,1))
  pairs(samples,pars = paste0(parameters[1:5],'_i[',i,']'))
  dev.off()
}

# RIC: choice+indifference points ---------------------
choice_ric <- choice_set%>%filter(t2>0,p2<1)
table(choice_ric$Exp)

indiff_ric <- read_csv("./RIC/data/previous/Indiff.csv")%>%
  add_column(t1=0,p1=1)%>%
  mutate(y=round(N/2))%>%
  rename(x1=Indifferences,x2=Amounts,t2=Delay,p2=Probability)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))%>%
  filter(t2>0,p2<1)
table(indiff_ric$Exp)

ric_set <- rbind(indiff_ric,choice_ric)

## individ ===============
parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr')
Set_list <- unique(ric_set$Exp)
Set_list
#for(i in Set_list){
i <- Set_list[1]
  ric_temp <- ric_set%>%filter(Exp==i)
  data<-list(
    nTrial=nrow(ric_temp),N = ric_temp$N,
    xs = ric_temp$xs,ts = ric_temp$ts, ps = ric_temp$ps,
    xd = ric_temp$xd,td = ric_temp$td, pd = ric_temp$pd,
    xr = ric_temp$xr,tr = ric_temp$tr, pr = ric_temp$pr,
    y = ric_temp$y)
  samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_ric.stan',
                  data=data,
                  pars=parameters,
                  iter = 2000,
                  warmup = 1000,
                  chains = 4, 
                  thin = 4,
                  cores = 4,
                  seed = 123,
                  verbose = TRUE,
                  refresh = 100,
                  control = list(max_treedepth = 15))
  saveRDS(samples,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 i,'.rds'))
#}
  pairs(samples)

## hier ================
Set_list <- unique(ric_set$Exp)
Set_list
ric_set$Exp_ind <- rep(0,length(ric_set$Exp))
for(i in 1:length(Set_list)){
  ric_set$Exp_ind[ric_set$Exp==Set_list[i]] <- i
}

parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'sd_i')

data<-list(
  nExp = length(Set_list),Exp = ric_set$Exp_ind,
  nTrial=nrow(ric_set),N = ric_set$N,
  xs = ric_set$xs,ts = ric_set$ts, ps = ric_set$ps,
  xd = ric_set$xd,td = ric_set$td, pd = ric_set$pd,
  xr = ric_set$xr,tr = ric_set$tr, pr = ric_set$pr,
  y = ric_set$y)

samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_ric.stan',
                  data=data,
                  pars=parameters,
                  iter = 2000,
                  warmup = 1000,
                  chains = 4, 
                  thin = 4,
                  cores = 4,
                  seed = 123,
                  verbose = TRUE,
                  refresh = 100,
                  control = list(max_treedepth = 20))
saveRDS(samples,
        './RIC/output/results/fit_prev/RITCH_indiff_ric.rds')

