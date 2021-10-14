rm(list=ls())
library(tidyverse)
library(rstan)
library(bayestestR)
options(mc.cores = parallel::detectCores())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')%>%
  mutate(xd = x1-x2, pd = p1-p2, td = t2-t1)%>%
  mutate(xs = sign(xd), ps = sign(pd), ts = sign(td),
         xr = 2*xd/(x1+x2), pr = 2*pd/(p1+p2),
         tr = ifelse(t1+t2==0,0,2*td/(t1+t2)))
# prior prediction ------------
data<-list(
  nPart = 100,
  nTrial=nrow(choice_set),
  xs = choice_set$xs,
  ps = choice_set$ps,
  ts = choice_set$ts,
  xd = choice_set$xd,
  pd = choice_set$pd,
  td = choice_set$td,
  xr = choice_set$xr,
  pr = choice_set$pr,
  tr = choice_set$tr
)
parameters <- 'ypred'
samples <- stan(file='./RIC/src/3_prior_prediction/prior_RITCH_ind.stan',
                data=data,
                pars=parameters,
                iter = 2000,
                warmup = 0,
                chains = 4,
                cores = 4,
                algorithm="Fixed_param")

saveRDS(samples, './RIC/output/results/prior_pred/prior_RITCH_ind.rds')

# core predictions -------------------
samples <- readRDS('./RIC/output/results/prior_pred/prior_RITCH_ind.rds')
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
## hdi of response ==============
hdi_ritch<-hdi(prop.1.Option,ci=0.99)
dim(hdi_ritch)
hdi_ritch<-hdi_ritch%>%
  add_column(model='RITCH',
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)

write_csv(hdi_ritch,'./RIC/output/results/prior_pred/hdi_ritch_ind.csv')
## hdi of manipulation effect ==============
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

eff_ritch <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
dim(eff_ritch)
hdi_eff_ritch <- hdi(eff_ritch,ci=0.99)
dim(hdi_eff_ritch)
hdi_eff_ritch <- hdi_eff_ritch%>%
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
write_csv(hdi_eff_ritch,'./RIC/output/results/prior_pred/hdi_eff_ritch_ind.csv')
