rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# normal priors --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/fit_prev/HD_param_choice.csv')
mu_post <- signif(post_param$mean,2)
sig_df <- read.csv('./RIC/src/4_core_pred_unfit/HD_sig.csv',header = T)
parameters <- 'ypred'

for(i in 1:4){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1, o2 = 1/choice_set$p2-1,
    mu_a = mu_post[1], mu_logh = mu_post[2],
    mu_i = mu_post[3], mu_s = mu_post[4],
    sig_a = sig_df[1,i], sig_logh = sig_df[2,i], 
    sig_i = sig_df[3,i], sig_s = sig_df[4,i])
    samples <- stan(file='./RIC/src/4_core_pred_unfit/prior_HD_normal.stan',
                data=data,
                pars=parameters,
                iter = 20000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/core_pred_unfit/prior_HD_normal_',i,'.rds'))
}

## hdi of response ============
rm(list=ls())
library(tidyverse)
library(bayestestR)

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

i=1
samples <- readRDS(paste0('./RIC/output/results/core_pred_unfit/prior_HD_normal_',i,'.rds'))
ypred <- rstan::extract(samples)$ypred
prop.1.Option <- matrix(data = NA,nrow = 20000,ncol = 384)
for(j in 1:20000)  prop.1.Option[j,] <- rowMeans(ypred[j,,])
prop.1.Option <- as.data.frame(prop.1.Option)
  
hdi_hd<-hdi(prop.1.Option,ci=0.9999)
hdi_hd<-hdi_hd%>%
    add_column(model='HD',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  
write_csv(hdi_hd,paste0('./RIC/output/results/core_pred_unfit/hdi_HD_normal_',i,'.csv'))

## hdi of manipulation effect ================
base_unfit <- choice_set$manipulation=='Base'
mag_unfit <- choice_set$manipulation=='Mag'
cert_unfit <- choice_set$manipulation=='Cert'
imm_unfit <- choice_set$manipulation=='Imm'

manip_eff <- data.frame(prop.1.Option[,mag_unfit] - prop.1.Option[,base_unfit])%>%
    bind_cols(data.frame(prop.1.Option[,cert_unfit] - prop.1.Option[,base_unfit]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_unfit] - prop.1.Option[,base_unfit]))
hdi_manip_eff <- hdi(manip_eff,ci=0.9999)
hdi_eff_hd <- hdi_manip_eff%>%as.data.frame()%>%
    add_column(model = 'HD',
               manipulation = c(choice_set$manipulation[mag_unfit],
                                choice_set$manipulation[cert_unfit],
                                choice_set$manipulation[imm_unfit]),
               choice = c(choice_set$choice[mag_unfit],choice_set$choice[cert_unfit],
                          choice_set$choice[imm_unfit]),
               trial_num = c(choice_set$num[mag_unfit],choice_set$num[cert_unfit],
                             choice_set$num[imm_unfit]),
               trial = c(choice_set$trial[mag_unfit],choice_set$trial[cert_unfit],
                         choice_set$trial[imm_unfit]))
write_csv(hdi_eff_hd,
            paste0('./RIC/output/results/core_pred_unfit/hdi_HD_eff_',i,'.csv'))
