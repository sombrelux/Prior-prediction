rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# prior pred --------------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
post_param <- read_csv('./RIC/output/results/data_prior/MHD_param.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

for(i in c(1,5,10,100)){
  data<-list(
    nPart = 100,
    nTrial=nrow(choice_set),
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1, o2 = 1/choice_set$p2-1,
    mu_a = mu_post[1], mu_c = mu_post[2], mu_loghd = mu_post[3], mu_loghr = mu_post[4], 
	mu_logsd = mu_post[5], mu_logsr = mu_post[6], mu_s = mu_post[7],
	sig_a = sig_post[1]*i, sig_c = sig_post[2]*i, sig_loghd = sig_post[3]*i, sig_loghr = sig_post[4]*i, 
	sig_logsd = sig_post[5]*i, sig_logsr = sig_post[6]*i, sig_s = sig_post[7]*i)
    samples <- stan(file='./RIC/src/4_data_prior/prior_MHD_ind.stan',
                data=data,
                pars=parameters,
                iter = 2000,
                warmup = 0,
                chains = 4,
                cores = 4,
                thin = 4,
                algorithm="Fixed_param")
	saveRDS(samples,paste0('./RIC/output/results/data_prior/prior_MHD_ind_',i,'.rds'))
}

# hdi of response ------------
samples <- readRDS('./RIC/output/results/data_prior/prior_MHD_ind.rds')
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option <- data.frame(apply(ypred,c(1,2),mean))
dim(prop.1.Option)

hdi_MHD <- hdi(prop.1.Option,ci=0.99)
dim(hdi_MHD)
hdi_MHD <- hdi_MHD%>%
  add_column(model='MHD',
             mean = apply(prop.1.Option,2,mean),
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)%>%
  group_by(manipulation,choice)%>%
  arrange(mean,.by_group = T)

write_csv(hdi_MHD,'./RIC/output/results/prior_pred/hdi_MHD_ind.csv')

## plot -----------

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

# hdi of manipulation effect -----------------
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

eff_MHD <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
dim(eff_MHD)
hdi_eff_MHD <- hdi(eff_MHD,ci=0.99)
dim(hdi_eff_MHD)
hdi_eff_MHD <- hdi_eff_MHD%>%
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
write_csv(hdi_eff_MHD,'./RIC/output/results/prior_pred/hdi_eff_MHD_ind.csv')

