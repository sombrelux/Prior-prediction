rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# choice --------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_pilot/PTT_postparam.csv')
mu_post <- signif(post_param$mean,2)
sig_df <- read.csv('./RIC/src/4_core_pred_pilot/PTT_sig.csv',header = T)
parameters <- 'ypred'

for(i in 1:4){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    p1 = choice_set$p1, p2 = choice_set$p2,
    mu_alpha = mu_post[1], mu_beta = mu_post[2], mu_gamma = mu_post[3], 
	  mu_R = mu_post[4], mu_s = mu_post[5],
    sig_alpha = sig_df[1,i], sig_beta = sig_df[2,i], 
	sig_gamma = sig_df[3,i], sig_R = sig_df[4,i], sig_s = sig_df[5,i])
    samples <- stan(file='./RIC/src/4_core_pred_pilot/prior_PTT_normal.stan',
                data=data,
                pars=parameters,
                iter = 20000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/core_pred_pilot/prior_PTT_normal_',i,'.rds'))
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
  samples <- readRDS(paste0('./RIC/output/results/core_pred_pilot/prior_PTT_normal_',i,'.rds'))
  ypred <- rstan::extract(samples)$ypred
  prop.1.Option <- matrix(data = NA,nrow = 20000,ncol = 384)
  for(j in 1:20000)  prop.1.Option[j,] <- rowMeans(ypred[j,,])
  prop.1.Option <- as.data.frame(prop.1.Option)
  hdi_ptt<-hdi(prop.1.Option,ci=0.9999)
  hdi_ptt<-hdi_ptt%>%as.data.frame()%>%
    add_column(model='PTT',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)
  write_csv(hdi_ptt,paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_normal_',i,'.csv'))
  
  manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
    bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
  hdi_manip_eff <- hdi(manip_eff,ci=0.9999)
  hdi_eff_ptt <- hdi_manip_eff%>%as.data.frame()%>%
    add_column(model = 'PTT',
               manipulation = c(choice_set$manipulation[mag_ind],
                                choice_set$manipulation[cert_ind],
                                choice_set$manipulation[imm_ind]),
               choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                          choice_set$choice[imm_ind]),
               trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                             choice_set$num[imm_ind]),
               trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                         choice_set$trial[imm_ind]))
  write_csv(hdi_eff_ptt,
            paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_eff_',i,'.csv'))
  
  rm(list = c('samples','ypred','prop.1.Option'))
}

