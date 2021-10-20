rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
#Sys.setenv(STAN_NUM_THREADS = 4)

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

data<-list(
  nPart = 100,
  nTrial=nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1)

parameters <- 'ypred'
samples <- stan(file='./RIC/src/4_data_prior/prior_HD_ind.stan',
                data=data,
                pars=parameters,
                iter = 2000,
                warmup = 0,
                chains = 4,
                cores = 4,
                algorithm="Fixed_param")

saveRDS(samples, './RIC/output/results/data_prior/prior_HD_ind.rds')

# hdi of response ------------
samples <- readRDS('./RIC/output/results/data_prior/prior_HD_ind.rds')
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,2),mean))
dim(prop.1.Option)

hdi_hd<-hdi(prop.1.Option,ci=0.99)
dim(hdi_hd)
hdi_hd<-hdi_hd%>%
  add_column(model='HD',
             mean = apply(prop.1.Option,2,mean),
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)%>%
  group_by(manipulation,choice)%>%
  arrange(mean,.by_group = T)

write_csv(hdi_hd,'./RIC/output/results/prior_pred/hdi_HD_ind.csv')

## plot -----------

hdi_hd<-hdi_hd%>%
  add_column(trial_sorted = rep(1:16,6*4))

hdi_hd[1:5,]
ggplot(hdi_hd,
       mapping = aes(x = trial_sorted,
                     group=manipulation)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill=manipulation), 
              alpha = 0.35) + 
  facet_wrap(~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="HD")

# hdi of manipulation effect -----------------
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

eff_hd <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
dim(eff_hd)
hdi_eff_hd <- hdi(eff_hd,ci=0.99)
dim(hdi_eff_hd)
hdi_eff_hd <- hdi_eff_hd%>%
  add_column(model = 'hd',
             manipulation = c(choice_set$manipulation[mag_ind],
                              choice_set$manipulation[cert_ind],
                              choice_set$manipulation[imm_ind]),
             choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                        choice_set$choice[imm_ind]),
             trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                           choice_set$num[imm_ind]),
             trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                       choice_set$trial[imm_ind]))
write_csv(hdi_eff_hd,'./RIC/output/results/prior_pred/hdi_eff_hd_ind.csv')

