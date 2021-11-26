rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# normal priors --------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_prev/PTT_param_choice.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

for(i in c(1,5,10,50,100)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    p1 = choice_set$p1, p2 = choice_set$p2,
    mu_alpha = mu_post[1], mu_beta = mu_post[2], mu_gamma = mu_post[3], mu_R = mu_post[4], mu_s = mu_post[5],
    sig_alpha = sig_post[1]*i, sig_beta = sig_post[2]*i, sig_gamma = sig_post[3]*i, sig_R = sig_post[4]*i, sig_s = sig_post[5]*i)
    samples <- stan(file='./RIC/src/4_core_pred/prior_PTT_normal.stan',
                data=data,
                pars=parameters,
                iter = 10000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/core_pred/prior_PTT_normal_',i,'.rds'))
}

## hdi of response ===================
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(1,5,10,50,100)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_PTT_normal_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_ptt<-hdi(prop.1.Option,ci=0.99)
  hdi_ptt<-hdi_ptt%>%
    add_column(model='PTT',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_ptt,
            paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
}

hdi_PTT<-hdi_PTT%>%
  add_column(trial_sorted = rep(1:16,6*4))

hdi_PTT[1:5,]
ggplot(hdi_PTT,
       mapping = aes(x = trial_sorted,
                     group=manipulation)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill=manipulation), 
              alpha = 0.35) + 
  facet_wrap(~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="PTT")


# uniform priors --------------
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_prev/PTT_param.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

for(i in c(3,15,30,300,600,1500)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    p1 = choice_set$p1, p2 = choice_set$p2,
    mu_alpha = mu_post[1], mu_beta = mu_post[2], mu_gamma = mu_post[3], mu_R = mu_post[4], mu_s = mu_post[5],
    half_alpha = sig_post[1]*i, half_beta = sig_post[2]*i, half_gamma = sig_post[3]*i, half_R = sig_post[4]*i, half_s = sig_post[5]*i)
  samples <- stan(file='./RIC/src/4_core_pred/prior_PTT_unif.stan',
                  data=data,
                  pars=parameters,
                  iter = 2000,
                  warmup = 0,
                  chains = 4,
                  cores = 4,
                  thin = 4,
                  algorithm="Fixed_param")
  saveRDS(samples,paste0('./RIC/output/results/core_pred/prior_PTT_unif_',i,'.rds'))
}


## hdi of response ================
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(3,15,30,300,600,1500)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_PTT_unif_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_hd<-hdi(prop.1.Option,ci=0.99)
  hdi_hd<-hdi_hd%>%
    add_column(model='PTT',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_hd,paste0('./RIC/output/results/core_pred/hdi_PTT_unif_',i,'.csv'))
}
