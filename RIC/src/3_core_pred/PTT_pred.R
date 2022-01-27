rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# choice --------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_pilot/PTT_postparam.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- read.csv('./RIC/src/3_core_pred/PTT_sig.csv',header = T)
parameters <- 'ypred'

for(i in 1:4){
  for(k in 1:5){
    data<-list(
      nPart = 100,
      nTrial=nrow(choice_set),
      x1 = choice_set$x1, x2 = choice_set$x2,
      t1 = choice_set$t1, t2 = choice_set$t2,
      p1 = choice_set$p1, p2 = choice_set$p2,
      mu_alpha = mu_post[1], mu_beta = mu_post[2], mu_gamma = mu_post[3], 
      mu_R = mu_post[4], mu_s = mu_post[5],
      sig_alpha = sig_post[1,i], sig_beta = sig_post[2,i], 
      sig_gamma = sig_post[3,i], sig_R = sig_post[4,i], sig_s = sig_post[5,i])
    samples <- stan(file='./RIC/src/3_core_pred/prior_PTT_normal.stan',
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
    write_csv(prop.1.Option,paste0('./RIC/output/results/core_pred/PTT',k,'_',i,'.csv'))
    rm(list = c('samples','ypred','prop.1.Option'))
  }
}

# ci ----------
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
    propk <- read_csv(paste0('./RIC/output/results/core_pred/PTT',k,'_',i,'.csv'))
    prop.1.Option <- rbind(prop.1.Option,propk)
  }
  
  hdi_ptt<-hdi(prop.1.Option,credMass=0.9999)
  hdi_ptt<-hdi_ptt%>%t()%>%data.frame()%>%
    add_column(model='PTT',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    rename(CI_low=lower,CI_high=upper)
  write_csv(hdi_ptt,paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
  
  manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
    bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
  hdi_manip_eff <- hdi(manip_eff,credMass = 0.9999)
  hdi_eff_ptt <- hdi_manip_eff%>%t()%>%data.frame()%>%
    add_column(model = 'PTT',
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
  write_csv(hdi_eff_ptt,
            paste0('./RIC/output/results/core_pred/hdi_PTT_eff_',i,'.csv'))
}

# plot ---------
rm(list=ls())
hdi_PTT <- NULL
for(i in 1:4){
  hdi_PTT_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_eff_',i,'.csv'))
  hdi_PTT_i$sigma <- i
  hdi_PTT <- rbind(hdi_PTT,hdi_PTT_i)
}
dim(hdi_PTT)

ggplot(hdi_PTT,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="PTT")

