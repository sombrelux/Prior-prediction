,
mu_beta_xt = mu_post[1], mu_beta_xp = mu_post[2],
mu_beta_xa = mu_post[3], mu_beta_xr = mu_post[4],
mu_beta_pa = mu_post[5], mu_beta_pr = mu_post[6],
mu_beta_ta = mu_post[7], mu_beta_tr = mu_post[8],
sig_beta_xt = sig_post[1], sig_beta_xp = sig_post[2],
sig_beta_xa = sig_post[3], sig_beta_xr = sig_post[4],
sig_beta_pa = sig_post[5], sig_beta_pr = sig_post[6],
sig_beta_ta = sig_post[7], sig_beta_tr = sig_post[8],
sigma = sigma
# delay =============
#for(i in choice_delay$Exp){
#i <- unique(choice_delay$Exp)[1]
#i  
#choice_temp <- choice_delay%>%filter(Exp==i)

choice_delay <- choice_set%>%
  filter(t2>0,p1==p2) #delay
dim(choice_delay) #1076   19
table(choice_delay$Exp)
all(choice_delay$xs<0)
all(choice_delay$ts>0)
# beta_o = -beta_xo+beta_to

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
choice_risky <- choice_set%>%filter(t1==t2,p2<1) 
dim(choice_risky) #227  18
table(choice_risky$Exp)
all(choice_risky$xs<0)
all(choice_risky$ps>0)

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

## choice ===============
parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr')
data<-list(
  nTrial=nrow(indiff_ric),N = indiff_ric$N,
  xs = indiff_ric$xs,ts = indiff_ric$ts, ps = indiff_ric$ps,
  xd = indiff_ric$xd,td = indiff_ric$td, pd = indiff_ric$pd,
  xr = indiff_ric$xr,tr = indiff_ric$tr, pr = indiff_ric$pr,
  y = indiff_ric$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_ric.stan',
                data=data,
                pars=parameters,
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
        './RIC/output/results/fit_prev/RITCH_group.rds')

post_stasts <- rstan::summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_ric_choice.csv')

png('./RIC/output/fig/fit_prev/Pairs_RITCH_ric_choice.png',
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters[1:9])
dev.off()

## individ ===============
parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr')
Set_list <- unique(indiff_ric$Exp)
Set_list
#for(i in Set_list){
i <- Set_list[4]
ric_temp <- indiff_ric%>%filter(Exp==i)
data<-list(
  nTrial=nrow(ric_temp),N = ric_temp$N,
  xs = ric_temp$xs,ts = ric_temp$ts, ps = ric_temp$ps,
  xd = ric_temp$xd,td = ric_temp$td, pd = ric_temp$pd,
  xr = ric_temp$xr,tr = ric_temp$tr, pr = ric_temp$pr,
  y = ric_temp$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_ric.stan',
                data=data,
                pars=parameters,
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
        paste0('./RIC/output/results/fit_prev/RITCH_',
               i,'.rds'))

post_stasts <- rstan::summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 i,'.csv'))

png(paste0('./RIC/output/fig/fit_prev/RITCH_',
           i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()
#}

# Pool -------------------
choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))
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

pool_set <- rbind(choice_set,indiff_ric)
pool_set$Exp_ind <- rep(0,length(pool_set$Exp))
Set_list <- unique(pool_set$Exp)
for(i in 1:length(Set_list)){
  pool_set$Exp_ind[pool_set$Exp==Set_list[i]] <- i
}


parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'sd_i','ypred')
data<-list(
  nExp = length(unique(pool_set$Exp)),
  nTrial=nrow(pool_set),
  Exp = pool_set$Exp_ind,
  N = pool_set$N,
  xs = pool_set$xs,ts = pool_set$ts, ps = pool_set$ps,
  xd = pool_set$xd,td = pool_set$td, pd = pool_set$pd,
  xr = pool_set$xr,tr = pool_set$tr, pr = pool_set$pr,
  y = pool_set$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_hier.stan',
                data=data,
                pars=parameters,
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
        './RIC/output/results/fit_prev/RITCH_pool_hier.rds')

post_stasts <- rstan::summary(samples)
post_stasts$summary[1:9,]
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_pool_hier.csv')

png('./RIC/output/fig/fit_prev/RITCH_pool_hier.png',
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters[1:9])
dev.off()

## post check =============
library(bayestestR)
samples <- readRDS('./RIC/output/results/fit_prev/RITCH_pool_hier.rds')
ypred <- extract(samples,pars='ypred')$ypred
dim(ypred)
hist(ypred[,2])
ypred <- as.data.frame(ypred)
hdi_ypred <- bayestestR::hdi(ypred)
dim(hdi_ypred)
hdi_ypred[1:10,]
pool_set$y[1:10]

post_pred <- data.frame(Exp_ind = pool_set$Exp_ind,
                        x = 1:nrow(pool_set),
                        y = pool_set$y,
                        CI_high = hdi_ypred$CI_high,
                        CI_low = hdi_ypred$CI_low)

library(ggplot2)
post_i <- post_pred%>%filter(Exp_ind==1)
ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))


# individ ----------------
Set_list <- unique(choice_set$Exp)
Set_list
i <- Set_list[10]
choice_temp <- choice_set%>%filter(Exp==i)
dim(choice_temp)
data <- list(
  nTrial = nrow(choice_temp),
  x1 = choice_temp$x1, x2 = choice_temp$x2,
  t1 = choice_temp$t1, t2 = choice_temp$t2,
  o1 = 1/choice_temp$p1-1,
  o2 = 1/choice_temp$p2-1,
  N = choice_temp$N,
  y = choice_temp$y)
parameters <- c('a','logh','i','logs',
                'ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_HD_ind.stan',
                data=data,
                pars=parameters,
                iter = 4000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15,
                               adapt_delta = 0.99))
saveRDS(samples,
        paste0('./RIC/output/results/data_prior/HD_',i,'.rds'))

## post inf ===============
png(paste0('./RIC/Output/fig/fit_prev/HD/pairs_',i,
           '.png'))
pairs(samples,pars = parameters[1:4])
dev.off()

png(paste0('./RIC/Output/fig/fit_prev/HD/trace_',i,
           '.png'))
traceplot(samples,pars = parameters[1:4])
dev.off()

## post pred =========
ypred <- extract(samples,pars='ypred')$ypred
ypred <- as.data.frame(ypred)
dim(ypred)
hdi_ypred <- bayestestR::hdi(ypred)
hist(ypred[,1])
data$y[1]
post_pred <- data.frame(y = data$y,
                        CI_high = hdi_ypred$CI_high,
                        CI_low = hdi_ypred$CI_low)%>%
  arrange(y)%>%
  add_column(x = 1:length(data$y))

#post_i <- post_pred%>%filter(Exp_ind==1)
ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title=i,x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('./RIC/Output/fig/fit_prev/HD/post_',i,'.png'),
       height = 6,width = 6)

# hier ----------------
Set_list <- unique(choice_set$Exp)
Set_list
choice_set$Exp_ind <- rep(0,length(choice_set$Exp))
for(i in 1:length(Set_list)){
  choice_set$Exp_ind[choice_set$Exp==Set_list[i]] <- i
}

data<-list(
  nExp = length(Set_list),
  Exp = choice_set$Exp_ind,
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = choice_set$N,
  y = choice_set$y)

parameters <- c('a','logh','i','s',
                'a_i','logh_i','i_i','s_i',
                'sd_i','ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_HD_hier.stan',
                data=data,
                pars=parameters,
                iter = 6000,
                warmup = 3000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
pairs(samples,pars = parameters[1:4])
traceplot(samples,pars = parameters[1:4])
saveRDS(samples,
        './RIC/output/results/data_prior/HD_hier.rds')

## hier ================
indiff_ric$Exp_ind <- rep(0,length(indiff_ric$Exp))
Set_list <- unique(indiff_ric$Exp)
for(i in 1:length(Set_list)){
  indiff_ric$Exp_ind[indiff_ric$Exp==Set_list[i]] <- i
}

parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'sd_i')

data<-list(
  nExp = length(Set_list),Exp = indiff_ric$Exp_ind,
  nTrial=nrow(indiff_ric),N = indiff_ric$N,
  xs = indiff_ric$xs,ts = indiff_ric$ts, ps = indiff_ric$ps,
  xd = indiff_ric$xd,td = indiff_ric$td, pd = indiff_ric$pd,
  xr = indiff_ric$xr,tr = indiff_ric$tr, pr = indiff_ric$pr,
  y = indiff_ric$y)

samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_ric_hier.stan',
                data=data,
                pars=parameters,
                iter = 4000,
                warmup = 2000,
                chains = 4, 
                thin = 4,
                cores = 4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 20))
traceplot(samples,pars=parameters[1:9])

saveRDS(samples,
        './RIC/output/results/fit_prev/RITCH_indiff_ric.rds')

post_stasts <- rstan::summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 i,'.csv'))
png(paste0('./RIC/output/results/fit_prev/RITCH_',
           i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters[1:9])
dev.off()


retr_set <- read_csv("./RIC/data/previous/Retrieval.csv")%>%
  add_column(Paradigm='Choice')%>%
  dplyr::select(Paradigm,Exp,x1,p1,t1,x2,p2,t2,N,y)
all(retr_set$x1<retr_set$x2)
retr_T <- retr_set%>%filter(t2>0,p2==1,p1==1) #delay
all(retr_T$t1<retr_T$t2)
retr_R <- retr_set%>%filter(t2==0,p2<1,t1==0) #risky
all(retr_R$p1>retr_R$p2)
retr_RI <- retr_set%>%filter(t2>0,p2<1) #ric
retr_RI

indiff_set <- read_csv("./RIC/data/previous/Indiff.csv")%>%
  add_column(Paradigm='Indiff',t1=0,p1=1)%>%
  mutate(y=round(N/2))%>%
  rename(x1=Indifferences,x2=Amounts,t2=Delay,p2=Probability)%>%
  dplyr::select(Paradigm,Exp,x1,p1,t1,x2,p2,t2,N,y)

all(indiff_set$x1<indiff_set$x2)
dim(indiff_set)
indiff_T <- indiff_set%>%filter(t2>0,p2==1) #delay
dim(indiff_T)
indiff_R <- indiff_set%>%filter(t2==0,p2<1) #risky
dim(indiff_R)
indiff_RI <- indiff_set%>%filter(t2>0,p2<1) #ric
dim(indiff_RI)

## combine ============

delay_set <- rbind(retr_T,indiff_T)
risky_set <- rbind(retr_R,indiff_R)
ric_set <- rbind(retr_RI,indiff_RI)

dim(delay_set)
dim(risky_set)
dim(ric_set)
unique(ric_set$Exp)

write_csv(delay_set,'./RIC/data/processed/prev_delay.csv')
write_csv(risky_set,'./RIC/data/processed/prev_risky.csv')
write_csv(ric_set,'./RIC/data/processed/prev_ric.csv')

# fit ric set -----------
ric_set <- read_csv('./RIC/data/processed/prev_ric.csv')

ric_set <- ric_set%>%


rm(list=ls())
library(tidyverse)
pw <- "./RIC/data/previous/Retrieval/"

# Reeck et al., 2017 ---------
load(paste0(pw,'Reeck17_E1.Rdata'))
Reeck17_E1 <- study1_payne%>%
  mutate(SS=dplyr::recode(choice,'ss'=1,'ll'=0),
         t2=dplyr::recode(ll_time,'4 weeks'=4,
                          '6 weeks'=6,
                          '2 weeks'=2),
         t1=dplyr::recode(ss_time,'0 weeks'=0,
                          '2 weeks'=2))%>%
  group_by(ll_amt_val)%>%
  summarise(N=n(),x1=mean(ss_amt_val),
            x2=mean(ll_amt_val),
            t1=mean(t1)/4,
            t2=mean(t2)/4,
            k=sum(SS))%>%
  add_column(Exp='Reeck17_E1',p1=1,p2=1)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
write_csv(Reeck17_E1,paste0(pw,'Reeck17_E1.csv'))

# CG 2016 ----------------
CG_E1 <- read_csv(paste0(pw,'CG16_E1.csv'))%>%
  mutate(k=round(N*(1-LP)))%>%
  add_column(t1=0,p1=1,p2=1,Exp='CG16_E1')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
CG_E2 <- read_csv(paste0(pw,'CG16_E2.csv'))%>%
  mutate(k=round(N*(1-LP)))%>%
  add_column(p1=1,p2=1,Exp='CG16_E2')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
CG_E3 <- read_csv(paste0(pw,'CG16_E3.csv'))%>%
  mutate(k=round(N*(1-LP)))%>%
  add_column(p1=1,p2=1,Exp='CG16_E3')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
CG16 <- rbind(CG_E1,CG_E2,CG_E3)
head(CG16)
write_csv(CG16,paste0(pw,'CG16.csv'))

# Ericson et al., 2015 ---------
# https://osf.io/z9wcj/
# time in weeks
ericson_set <- 
  read_csv(paste0(pw,"Ericson_et_al_2015.csv"))

intertemp_set <- ericson_set%>%
  filter(Condition==3,
         !is.na(LaterOptionChosen))%>%
  mutate(P=1-LaterOptionChosen,
         t1=T1/4,t2=T2/4)%>%
  filter(X1<15000,X2<15000)

all(intertemp_set$X1-intertemp_set$X2<0)
sort(unique(intertemp_set$X1))

group_id<-intertemp_set%>%
  group_by(Subject)%>%
  group_indices()

question_id<-intertemp_set%>%
  group_by(X1,X2,t1,t2)%>%
  group_indices()

intertemp_set<-intertemp_set%>%
  add_column(ID=group_id,Choice=question_id)

group_result<-intertemp_set%>%
  group_by(Choice)%>%
  summarise(x1=mean(X1),x2=mean(X2),
            N = n(),t1=mean(t1),t2=mean(t2),
            k=sum(P))%>%
  add_column(p1=1,p2=1,Exp='Ericson15')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
write_csv(group_result,paste0(pw,'Ericson15.csv'))

# SR 2014 ------------
SR_set1 <- 
  read_csv(paste0(pw,"SR14.csv"))%>%
  mutate(k=round(N*(1-LR)))%>%
  add_column(t1=0,t2=0,p1=1)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
write_csv(SR_set1,paste0(pw,'SR14.csv'))

# SRS 2014 -------------
SRS_E2 <- 
  read_csv(paste0(pw,"SRS14_E2.csv"))%>%
  mutate(P=1/(`odd-LL`+1))%>%
  mutate(k=round(N*P))%>%
  add_column(p1=1,p2=1,Exp='SRS14_E2')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
SRS_E3 <- 
  read_csv(paste0(pw,"SRS14_E3.csv"))%>%
  mutate(P=1/(`odd-LL`+1))%>%
  mutate(k=round(N*P))%>%
  add_column(t1=0,p1=1,p2=1,Exp='SRS14_E3')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
SRS14 <- rbind(SRS_E2,SRS_E3)
SRS14
write_csv(SRS14,paste0(pw,'SRS14.csv'))

# SR 2010 -------------
SR_set2 <- read_csv(paste0(pw,'SR10.csv'))
k1 <- SR_set2$N*SR_set2$o
k1
k <- c(k1[1:8],SR_set2$k[9:11])
Exp <- paste0('SR10_E',SR_set2$Exp)
Exp
SR_set2 <- SR_set2%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,N)%>%
  add_column(Exp,k)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
SR_set2  
write_csv(SR_set2,paste0(pw,'SR10.csv'))

# WC 2005 ------------
WC05 <- read_csv(paste0(pw,'WC05.csv'))%>%
  mutate(k=round(N*prop))
Exp <- paste0('WC05_',WC05$table)
WC05 <- WC05%>%add_column(Exp)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
WC05
write_csv(WC05,paste0(pw,'WC05.csv'))

# GW 2002,1992 ------------
## Exp in 1992: 20 subj, each 10 times
## Exp in 2002: 7 subj in analysis, each 20 time
GW_set <- read_csv(paste0(pw,'GW2002.csv'))
GW_set <- GW_set%>%
  mutate(k=round(n*o))%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,n,k)
GW_set$Exp <- paste0('GW',GW_set$Exp)
write_csv(GW_set,paste0(pw,'GW02.csv'))

# Erev et al. 2002 -----------
erev_set <- read_csv(paste0(pw,"Erev2002.csv"))
x1 <- erev_set$v1
x2 <- erev_set$v2
x1[erev_set$v1>erev_set$v2] <- erev_set$v2[erev_set$v1>erev_set$v2]
x2[erev_set$v1>erev_set$v2] <- erev_set$v1[erev_set$v1>erev_set$v2]
all(x1<=x2)

p1 <- erev_set$p1
p2 <- erev_set$p2
p1[erev_set$v1>erev_set$v2] <- erev_set$p2[erev_set$v1>erev_set$v2]
p2[erev_set$v1>erev_set$v2] <- erev_set$p1[erev_set$v1>erev_set$v2]
all(p1>=p2)

theta <- erev_set$o
theta[erev_set$v1>erev_set$v2] <- 1-theta[erev_set$v1>erev_set$v2] 

Erev_df <- data.frame(x1,p1,x2,p2,theta)%>%
  mutate(N=38,
         k=round(38*theta))%>%
  add_column(t1=0,t2=0)

Erev_df <- Erev_df%>%
  add_column(Exp='Erev02')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
head(Erev_df)
write_csv(Erev_df,paste0(pw,'Erev_02.csv'))
