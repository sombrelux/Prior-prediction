rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# normal priors --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_prev/HD_param_choice.csv')
post_param

mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

for(i in c(1,5,10,50,100)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1, o2 = 1/choice_set$p2-1,
    mu_a = mu_post[1], mu_logh = mu_post[2], mu_i = mu_post[3], mu_s = mu_post[4],
    sig_a = sig_post[1]*i, sig_logh = sig_post[2]*i, sig_i = sig_post[3]*i, sig_s = sig_post[4]*i)
    samples <- stan(file='./RIC/src/4_core_pred/prior_HD_normal.stan',
                data=data,
                pars=parameters,
                iter = 10000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/core_pred/prior_HD_normal_',i,'.rds'))
}

## hdi of response ============
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(1,5,10,50,100)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_HD_normal_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  hdi_hd<-hdi(prop.1.Option,ci=0.99)
  hdi_hd<-hdi_hd%>%
    add_column(model='HD',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
  write_csv(hdi_hd,paste0('./RIC/output/results/core_pred/hdi_HD_normal_',i,'.csv'))
}

## hdi of manipulation effect ================
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

for(i in c(1,5,10,50,100)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred/prior_HD_normal_',i,'.rds'))
  ypred <- extract(samples)$ypred
  prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
  manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
    bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
  hdi_manip_eff <- hdi(manip_eff,ci=0.99)
  hdi_eff_hd <- hdi_manip_eff%>%
    add_column(model = 'HD',
               manipulation = c(choice_set$manipulation[mag_ind],
                                choice_set$manipulation[cert_ind],
                                choice_set$manipulation[imm_ind]),
               choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                          choice_set$choice[imm_ind]),
               trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                             choice_set$num[imm_ind]),
               trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                         choice_set$trial[imm_ind]))
  write_csv(hdi_eff_hd,
            paste0('./RIC/output/results/core_pred/hdi_eff_hd_',i,'.csv'))
}

