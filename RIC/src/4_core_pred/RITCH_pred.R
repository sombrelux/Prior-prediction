rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# normal priors --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))

post_param <- read_csv('./RIC/output/results/fit_prev/RITCH_param_choice.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

for(i in c(1,5,10,50,100 )){
  for(sig_beta_xo in c(0.01,0.05,0.1,0.5)){
    data<-list(
      nPart = 100,
      nTrial=nrow(choice_set),
      xs = choice_set$xs, ps = choice_set$ps, ts = choice_set$ts,
      xd = choice_set$xd, pd = choice_set$pd, td = choice_set$td,
      xr = choice_set$xr, pr = choice_set$pr, tr = choice_set$tr,
      mu_beta_xt = mu_post[1], mu_beta_xp = mu_post[2], 
      mu_beta_xa = mu_post[3], mu_beta_xr = mu_post[4],
      mu_beta_pa = mu_post[5], mu_beta_pr = mu_post[6],
      mu_beta_ta = mu_post[7], mu_beta_tr = mu_post[8],
      sig_beta_xo = sig_beta_xo,
      sig_beta_xt = sig_post[1]*i, sig_beta_xp = sig_post[2]*i, 
      sig_beta_xa = sig_post[3]*i, sig_beta_xr = sig_post[4]*i,
      sig_beta_pa = sig_post[5]*i, sig_beta_pr = sig_post[6]*i,
      sig_beta_ta = sig_post[7]*i, sig_beta_tr = sig_post[8]*i)
    samples <- stan(file='./RIC/src/4_core_pred/prior_RITCH_normal.stan',
                    data=data,
                    pars=parameters,
                    iter = 20000,
                    warmup = 0,
                    chains = 4,
                    cores = 4,
                    thin = 4,
                    algorithm="Fixed_param")
    
    saveRDS(samples,
            paste0('./RIC/output/results/core_pred/RITCH_normal_',i,'_',sig_beta_xo,'.rds')) 
  }
}

## hdi of response =============
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(1,5,10,50,100)){
  for(sig_beta_xo in c(0.01,0.05,0.1,0.5)){
    samples <- readRDS(paste0('./RIC/output/results/core_pred/RITCH_normal_',i,'_',sig_beta_xo,'.rds'))
    ypred <- extract(samples)$ypred
    prop.1.Option<-data.frame(ypred/100)
  
    hdi_ritch <- hdi(prop.1.Option,ci=0.9999)
    hdi_ritch <- hdi_ritch%>%
      add_column(model='RITCH',
                 mean = apply(prop.1.Option,2,mean),
                 manipulation=choice_set$manipulation,
                 choice=choice_set$choice,
                 trial_num=choice_set$num,
                 trial=choice_set$trial)%>%
      group_by(manipulation,choice)%>%
      arrange(mean,.by_group = T)
    write_csv(hdi_ritch,paste0('./RIC/output/results/core_pred/hdi_RITCH_normal_',i,'_',sig_beta_xo,'.csv'))
  }
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
  for(sig_beta_xo in c(0.01,0.1,0.5,1)){
    samples <- readRDS(paste0('./RIC/output/results/core_pred/RITCH_normal_',i,'_',sig_beta_xo,'.rds'))
    ypred <- extract(samples)$ypred
    prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
  
    manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
      bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
      bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
    hdi_manip_eff <- hdi(manip_eff,ci=0.99)
    hdi_eff_ritch <- hdi_manip_eff%>%
      add_column(model = 'RITCH',
                 manipulation = c(choice_set$manipulation[mag_ind],
                                choice_set$manipulation[cert_ind],
                                choice_set$manipulation[imm_ind]),
                 choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                          choice_set$choice[imm_ind]),
                 trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                             choice_set$num[imm_ind]),
                 trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                         choice_set$trial[imm_ind]))
    write_csv(hdi_eff_ritch,
            paste0('./RIC/output/results/core_pred/hdi_eff_ritch_',i,'_',sig_beta_xo,'.csv'))

  }
}

