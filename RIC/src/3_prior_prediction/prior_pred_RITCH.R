rm(list=ls())
library(tidyverse)
library(rstan)
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

saveRDS(samples,
        paste0('./RIC/output/results/prior_pred/prior_RITCH_ind.rds'))

# core predictions -------------------
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_ritch<-hdi(prop.1.Option,ci=0.99)
hdi_ritch<-hdi_ritch%>%
  add_column(model='RITCH',
             true=m,
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)