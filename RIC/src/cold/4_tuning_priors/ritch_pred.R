source('./RIC/src/requires.R')
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

i = 4
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'
man_ind <- mag_ind+cert_ind+imm_ind==1

choice_resp <- read_csv("./RIC/data/processed/choice_resp.csv")
m <- colMeans(choice_resp)
eff_true <- c(m[mag_ind]-m[base_ind],
              m[cert_ind]-m[base_ind],
              m[imm_ind]-m[base_ind])

# pred ------------
data<-list(
  nPart = 100,
  nTrial=nrow(choice_set),
  xs = sign(choice_set$x1 - choice_set$x2),
  ps = sign(choice_set$p1 - choice_set$p2),
  ts = sign(choice_set$t2 - choice_set$t1),
  xd = choice_set$x1 - choice_set$x2,
  xr = 2*(choice_set$x1 - choice_set$x2)/(choice_set$x1 + choice_set$x2),
  pd = choice_set$p1 - choice_set$p2,
  pr = 2*(choice_set$p1 - choice_set$p2)/(choice_set$p1 + choice_set$p2),
  td = choice_set$t2 - choice_set$t1,
  tr = 2*(choice_set$t2 - choice_set$t1)/(choice_set$t1 + choice_set$t2)
)
data$tr[is.na(data$tr)] <- 0
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/5_tuning_ritch/prior_RITCH_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/prior_prediction/prior_',i,'/RITCH_exp.rds'))

# core ------------
samples <- readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/RITCH_exp.rds'))
          
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_ritch<-hdi(prop.1.Option,ci=0.99)
hdi_ritch<-hdi_ritch%>%
  add_column(model='RITCH',
             true=m,
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)

eff_RITCH <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
hdi_RITCH_eff <- hdi(eff_RITCH,ci=0.99)
hdi_RITCH_eff<-hdi_RITCH_eff%>%
  add_column(model='RITCH',
             true=eff_true,
             manipulation=choice_set$manipulation[man_ind],
             choice=choice_set$choice[man_ind],
             trial_num=choice_set$num[man_ind],
             trial=choice_set$trial[man_ind])
  
data_prior <- readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/data_prior.rds'))
hdi_model <- rbind(hdi_ritch,data_prior)
saveRDS(hdi_model, paste0('./RIC/output/results/prior_prediction/prior_',i,'/hdi_model.rds'))

eff_prior <- readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/eff_prior.rds'))
hdi_eff <- rbind(hdi_RITCH_eff,eff_prior)
saveRDS(hdi_eff, paste0('./RIC/output/results/prior_prediction/prior_',i,'/hdi_eff.rds'))
