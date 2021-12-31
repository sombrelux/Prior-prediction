rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# choice --------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_pilot/MHD_postparam.csv')
mu_post <- signif(post_param$mean,2)
sig_df <- read.csv('./RIC/src/4_core_pred_pilot/MHD_sig.csv',header = T)
parameters <- 'ypred'

for(i in 1:4){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1, o2 = 1/choice_set$p2-1,
    mu_a = mu_post[1], mu_c = mu_post[2],
    mu_loghd = mu_post[3], mu_loghr = mu_post[4], 
	  mu_sd = mu_post[5], mu_sr = mu_post[6], 
    mu_s = mu_post[7],
	  sig_a = sig_df[1,i], sig_c = sig_df[2,i], 
    sig_loghd = sig_df[3,i], sig_loghr = sig_df[4,i],
	  sig_sd = sig_df[5,i], sig_sr = sig_df[6,i], 
    sig_s = sig_df[7,i])
    samples <- stan(file='./RIC/src/4_core_pred_pilot/prior_MHD_normal.stan',
                data=data,
                pars=parameters,
                iter = 20000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/core_pred_pilot/prior_MHD_normal_',i,'.rds'))
}

## hdi ============
rm(list=ls())
library(tidyverse)
library(bayestestR)

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

for(i in 1:4){
  samples <- readRDS(paste0('./RIC/output/results/core_pred_pilot/prior_MHD_normal_',i,'.rds'))
  ypred <- rstan::extract(samples)$ypred
  prop.1.Option <- matrix(data = NA,nrow = 20000,ncol = 384)
  for(j in 1:20000)  prop.1.Option[j,] <- rowMeans(ypred[j,,])
  prop.1.Option <- as.data.frame(prop.1.Option)
  hdi_mhd<-hdi(prop.1.Option,ci=0.9999)
  hdi_mhd<-hdi_mhd%>%as.data.frame()%>%
    add_column(model='MHD',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)
  write_csv(hdi_mhd,paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_normal_',i,'.csv'))
  
  manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
    bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
  hdi_manip_eff <- hdi(manip_eff,ci=0.9999)
  hdi_eff_mhd <- hdi_manip_eff%>%as.data.frame()%>%
    add_column(model = 'MHD',
               manipulation = c(choice_set$manipulation[mag_ind],
                                choice_set$manipulation[cert_ind],
                                choice_set$manipulation[imm_ind]),
               choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                          choice_set$choice[imm_ind]),
               trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                             choice_set$num[imm_ind]),
               trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                         choice_set$trial[imm_ind]))
  write_csv(hdi_eff_mhd,
            paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'.csv'))
  
  rm(list = c('samples','ypred','prop.1.Option'))
}

