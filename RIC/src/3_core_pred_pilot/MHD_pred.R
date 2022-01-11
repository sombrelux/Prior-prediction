rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# fit -------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_pilot/MHD_postparam.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- read.csv('./RIC/src/3_core_pred_pilot/MHD_sig.csv',header = T)
parameters <- 'ypred'

for(i in 1:4){
  for(k in 1:5){
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
      sig_a = sig_post[1,i], sig_c = sig_post[2,i], 
      sig_loghd = sig_post[3,i], sig_loghr = sig_post[4,i],
      sig_sd = sig_post[5,i], sig_sr = sig_post[6,i], 
      sig_s = sig_post[7,i])
    samples <- stan(file='./RIC/src/3_core_pred_pilot/prior_MHD_normal.stan',
                    data=data,
                    pars=parameters,
                    iter = 1000,
                    warmup = 0,
                    chains = 20,
                    cores = 20,
                    algorithm="Fixed_param")
    ypred <- rstan::extract(samples)$ypred
    prop.1.Option <- matrix(data = NA,nrow = 20000,ncol = 384)
    for(j in 1:20000)  prop.1.Option[j,] <- rowMeans(ypred[j,,])
    prop.1.Option <- as.data.frame(prop.1.Option)
    write_csv(prop.1.Option,paste0('./RIC/output/results/core_pred_pilot/MHD',k,'_',i,'.csv'))
    rm(list = c('samples','ypred','prop.1.Option'))
  }
}

# ci ---------
rm(list=ls())
library(tidyverse)
library(HDInterval)

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

for(i in 1:4){
  prop.1.Option <- NULL
  for(k in 1:5){
    propk <- read_csv(paste0('./RIC/output/results/core_pred_pilot/MHD',k,'_',i,'.csv'))
    prop.1.Option <- rbind(prop.1.Option,propk)
  }
  
  hdi_mhd<-hdi(prop.1.Option,credMass=0.9999)
  hdi_mhd<-hdi_mhd%>%t()%>%data.frame()%>%
    add_column(model='MHD',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    rename(CI_low=lower,CI_high=upper)
  write_csv(hdi_mhd,paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_normal_',i,'.csv'))
  
  manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
    bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
  hdi_manip_eff <- hdi(manip_eff,credMass = 0.9999)
  hdi_eff_mhd <- hdi_manip_eff%>%t()%>%data.frame()%>%
    add_column(model = 'MHD',
               manipulation = c(choice_set$manipulation[mag_ind],
                                choice_set$manipulation[cert_ind],
                                choice_set$manipulation[imm_ind]),
               choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                          choice_set$choice[imm_ind]),
               trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                             choice_set$num[imm_ind]),
               trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                         choice_set$trial[imm_ind]))%>%
    rename(CI_low=lower,CI_high=upper)
  write_csv(hdi_eff_mhd,
            paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'.csv'))
}

# plot ----------
rm(list=ls())
hdi_mhd <- NULL
for(i in 1:4){
  hdi_mhd_i <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'0.csv'))
  hdi_mhd_i$sigma <- i
  hdi_mhd <- rbind(hdi_mhd,hdi_mhd_i)
}
dim(hdi_mhd)

ggplot(hdi_mhd,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="MHD")
