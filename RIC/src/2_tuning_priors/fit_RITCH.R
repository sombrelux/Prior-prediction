rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# available data sets ------------
## money below 15000, delay below 6 years
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
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

ric_set$Exp_ind <- rep(0,length(ric_set$Exp))
Set_list <- unique(ric_set$Exp)
for(i in 1:length(Set_list)){
  ind <- ric_set$Exp==Set_list[i]
  ric_set$Exp_ind[ind] <- i
}
parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'SD_i')

data<-list(
  nTrial=nrow(ric_set),nExp=length(Set_list),
  N = ric_set$N, Exp = ric_set$Exp_ind,
  xs = ric_set$xs,ts = ric_set$ts,ps = ric_set$ps,
  xd = ric_set$xd,td = ric_set$td,pd = ric_set$pd,
  xr = ric_set$xr,tr = ric_set$tr,pr = ric_set$pr,
  y = ric_set$y)

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
saveRDS(samples, './RIC/output/results/fit_prev/RITCH_ric.rds')
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_prev/RITCH_ric_stats.csv')

## posterior ==================
png("./RIC/output/fig/fit_prev/RITCH_ric_pairs.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters)
dev.off()

png("./RIC/output/fig/fit_prev/RITCH_ric_trace.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
traceplot(samples, pars = parameters)
dev.off()



dim(indiff_set)
indiff_set <- indiff_set%>%
				mutate(xs = -1,ts = sign(Delay),ps = sign(1-Probability),
				xd = Indifferences - Amounts,
				td = Delay, pd = 1-Probability,
				xr = 2*(Indifferences - Amounts)/(Indifferences + Amounts),
				tr = ifelse(Delay==0,0,2),
				pr = 2*(1-Probability)/(1+Probability),
				y = round(N*0.5))

indiff_set$Exp_ind <- rep(0,length(indiff_set$Exp))
Set_list <- unique(indiff_set$Exp)
for(i in 1:length(Set_list)){
  ind <- indiff_set$Exp==Set_list[i]
  indiff_set$Exp_ind[ind] <- i
}
parameters <- c('beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr',
                'SD_i')

data<-list(
	nTrial=nrow(indiff_set),nExp=length(Set_list),
	N = indiff_set$N, Exp = indiff_set$Exp_ind,
	xs = indiff_set$xs,ts = indiff_set$ts,ps = indiff_set$ps,
	xd = indiff_set$xd,td = indiff_set$td,pd = indiff_set$pd,
	xr = indiff_set$xr,tr = indiff_set$tr,pr = indiff_set$pr,
	y = indiff_set$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_indiff.stan',
                  data=data,
                  pars=parameters,
                  iter=6000,
                  warmup = 2000,
                  chains=4, 
                  thin=4,
                  cores=4,
                  seed = 123,
                verbose = TRUE,
                refresh = 50,
                control = list(max_treedepth = 15))
saveRDS(samples,
          paste0(pw,"RITCH_indiff.rds"))
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0(pw,'RITCH_indiff_stats.csv'))
## posterior ==================
fit_indiff <- readRDS(paste0(pw,"RITCH_indiff.rds"))
png("./RIC/output/fig/RITCH_indiff_pairs.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(fit_indiff,pars = parameters)
dev.off()

png("./RIC/output/fig/RITCH_indiff_trace.png",
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
traceplot(fit_indiff, pars = parameters)
dev.off()
