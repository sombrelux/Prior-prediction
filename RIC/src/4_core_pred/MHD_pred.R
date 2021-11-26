rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# normal priors --------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_prev/MHD_param_choice.csv')
post_param
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- c('a','c','logh_d','logh_r',
                's_d','s_r','s','ypred')

for(i in c(1,5,10,20,50,100)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1, o2 = 1/choice_set$p2-1,
    mu_a = mu_post[1], mu_c = mu_post[2], 
    #mu_loghd = mu_post[3], mu_loghr = mu_post[4], 
    mu_loghd = 0, mu_loghr = 0, #peak at 0
	  mu_logsd = mu_post[5], mu_logsr = mu_post[6], 
    mu_s = mu_post[7],
	  sig_a = sig_post[1]*i, sig_c = sig_post[2]*i, 
    sig_loghd = sig_post[3]*i, sig_loghr = sig_post[4]*i, 
	  sig_logsd = sig_post[5]*i, sig_logsr = sig_post[6]*i, 
    sig_s = sig_post[7]*i)
    samples <- stan(file='./RIC/src/4_core_pred/prior_MHD_normal.stan',
                data=data,
                pars=parameters,
                iter = 10000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/core_pred/prior_MHD_normal_',i,'.rds'))
}

## hdi of response ==============
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(1,5,10,20,50,100)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_MHD_normal_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_mhd<-hdi(prop.1.Option,ci=0.99)
  hdi_mhd<-hdi_mhd%>%
    add_column(model='MHD',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_mhd,paste0('./RIC/output/results/core_pred/hdi_MHD_normal_',i,'.csv'))
}

hdi_MHD<-hdi_mhd%>%
  add_column(trial_sorted = rep(1:16,6*4))

hdi_MHD[1:5,]
ggplot(hdi_MHD,
       mapping = aes(x = trial_sorted,
                     group=manipulation)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill=manipulation), 
              alpha = 0.35) + 
  facet_wrap(~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="MHD")

# uniform priors --------------
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_prev/MHD_param.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

for(i in c(3,15,30,60,150,300,600,1500)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1, o2 = 1/choice_set$p2-1,
    mu_a = mu_post[1], mu_c = mu_post[2], mu_loghd = mu_post[3], mu_loghr = mu_post[4], 
    mu_logsd = mu_post[5], mu_logsr = mu_post[6], mu_s = mu_post[7],
    half_a = sig_post[1]*i, half_c = sig_post[2]*i, half_loghd = sig_post[3]*i, half_loghr = sig_post[4]*i, 
    half_logsd = sig_post[5]*i, half_logsr = sig_post[6]*i, half_s = sig_post[7]*i)
  samples <- stan(file='./RIC/src/4_core_pred/prior_MHD_unif.stan',
                  data=data,
                  pars=parameters,
                  iter = 2000,
                  warmup = 0,
                  chains = 4,
                  cores = 4,
                  thin = 4,
                  algorithm="Fixed_param")
  saveRDS(samples,paste0('./RIC/output/results/core_pred/prior_MHD_unif_',i,'.rds'))
}

## hdi of response ==============
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(3,15,30,60,150,300,600,1500)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_MHD_unif_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_mhd<-hdi(prop.1.Option,ci=0.99)
  hdi_mhd<-hdi_mhd%>%
    add_column(model='MHD',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_mhd,paste0('./RIC/output/results/core_pred/hdi_MHD_unif_',i,'.csv'))
}

## plot ===============

hdi_MHD<-hdi_MHD%>%
  add_column(trial_sorted = rep(1:16,6*4))

hdi_MHD[1:5,]
ggplot(hdi_MHD,
       mapping = aes(x = trial_sorted,
                     group=manipulation)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill=manipulation), 
              alpha = 0.35) + 
  facet_wrap(~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="MHD")