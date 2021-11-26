rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# original priors --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

parameters <- 'ypred'
data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    xs = choice_set$xs, ps = choice_set$ps, ts = choice_set$ts,
    xd = choice_set$xd, pd = choice_set$pd, td = choice_set$td,
    xr = choice_set$xr, pr = choice_set$pr, tr = choice_set$tr)
samples <- stan(file='./RIC/src/4_core_pred/prior_RITCH_origin.stan',
                  data=data,
                  pars=parameters,
                  iter = 2000,
                  warmup = 0,
                  chains = 4,
                  cores = 4,
                  thin = 4,
                  algorithm="Fixed_param")
  
  saveRDS(samples,
          paste0('./RIC/output/results/core_pred/prior_RITCH_unif_',i,'.rds'))
}

## hdi of response =============
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(3,15,30,300,600,1500)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_RITCH_unif_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_hd<-hdi(prop.1.Option,ci=0.99)
  hdi_hd<-hdi_hd%>%
    add_column(model='RITCH',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_hd,paste0('./RIC/output/results/core_pred/hdi_RITCH_unif_',i,'.csv'))
}

# normal priors --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

#post_param <- read_csv('./RIC/output/results/fit_prev/RITCH_param_choice.csv')
#mu_post <- signif(post_param$mean,2)
#sig_post <- signif(post_param$sd,2)

post_param2 <- read_csv('./RIC/output/results/fit_prev/RITCH_param_indiff.csv')
post_param2
mu_post <- signif(post_param2$mean,2)
sig_post <- signif(post_param2$sd,2)
parameters <- 'ypred'

for(i in c(1,5,10,50,100)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    xs = choice_set$xs, ps = choice_set$ps, ts = choice_set$ts,
    xd = choice_set$xd, pd = choice_set$pd, td = choice_set$td,
    xr = choice_set$xr, pr = choice_set$pr, tr = choice_set$tr,
  	mu_beta_xo = mu_post[3], mu_beta_xa = mu_post[6], mu_beta_xr = mu_post[7],
  	mu_beta_po = mu_post[5], mu_beta_pa = mu_post[8], mu_beta_pr = mu_post[9],
  	mu_beta_to = mu_post[4], mu_beta_ta = mu_post[10], mu_beta_tr = mu_post[11],
  	sig_beta_xo = sig_post[3]*i, sig_beta_xa = sig_post[6]*i, sig_beta_xr = sig_post[7]*i,
  	sig_beta_po = sig_post[5]*i, sig_beta_pa = sig_post[8]*i, sig_beta_pr = sig_post[9]*i,
  	sig_beta_to = sig_post[4]*i, sig_beta_ta = sig_post[10]*i, sig_beta_tr = sig_post[11]*i)
  samples <- stan(file='./RIC/src/4_core_pred/prior_RITCH_normal.stan',
                data=data,
                pars=parameters,
                iter = 2000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")

  saveRDS(samples,
          paste0('./RIC/output/results/core_pred/prior_RITCH_normal_',i,'.rds'))
}

## hdi of response =============
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(1,5,10,50,100)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_RITCH_normal_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_hd<-hdi(prop.1.Option,ci=0.99)
  hdi_hd<-hdi_hd%>%
    add_column(model='RITCH',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_hd,paste0('./RIC/output/results/core_pred/hdi_RITCH_normal_',i,'.csv'))
}
