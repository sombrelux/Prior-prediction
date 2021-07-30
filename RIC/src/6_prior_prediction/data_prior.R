source('./RIC/src/requires.R')
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

i <- 4
if(!dir.exists(paste0('./RIC/output/results/prior_prediction/prior_',i))){
  dir.create(paste0('./RIC/output/results/prior_prediction/prior_',i))
}

# prior pred --------
## HD ------------
data <- list(
  nPart = 100,
  nTrial=nrow(choice_set),
  x1=choice_set$x1,
  x2=choice_set$x2,
  t1=choice_set$t1,
  t2=choice_set$t2,
  o1=1/choice_set$p1-1,
  o2=1/choice_set$p2-1)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_HD_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/prior_prediction/prior_',i,'/HD_exp.rds'))

## MHD --------------------
data <- list(
  nPart = 100,
  nTrial=nrow(choice_set),
  x1=choice_set$x1,
  x2=choice_set$x2,
  t1=choice_set$t1,
  t2=choice_set$t2,
  o1=1/choice_set$p1-1,
  o2=1/choice_set$p2-1)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_MHD_',i,'.stan'),
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/prior_prediction/prior_',i,'/MHD_exp.rds'))

## PTT -------------
data <- list(
  nPart = 100,
  nTrial=nrow(choice_set),
  x1=choice_set$x1,
  x2=choice_set$x2,
  t1=choice_set$t1,
  t2=choice_set$t2,
  p1=choice_set$p1,
  p2=choice_set$p2)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_PTT_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/prior_prediction/prior_',i,'/PTT_exp.rds'))

# DATA PRIOR ----------
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
## hd ===========
samples <- 
  readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/HD_exp.rds'))
ypred <- extract(samples)$ypred
prop.1.Option <- data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
### proportion ===========
hdi_hd<-hdi(prop.1.Option,ci=0.99)
hdi_hd<-hdi_hd%>%
  add_column(model='HD',true=m,
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)
### Effects ==============
eff_hd <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
hdi_hd_eff <- hdi(eff_hd,ci=0.99)
hdi_hd_eff<-hdi_hd_eff%>%
  add_column(model='HD',
             true=eff_true,
             manipulation=choice_set$manipulation[man_ind],
             choice=choice_set$choice[man_ind],
             trial_num=choice_set$num[man_ind],
             trial=choice_set$trial[man_ind])
## ptt =========
samples <- 
  readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/PTT_exp.rds'))

ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
### proportion ===========
hdi_ptt<-hdi(prop.1.Option,ci=0.99)
hdi_ptt<-hdi_ptt%>%
  add_column(model='PTT',
             true=m,
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)
### Effects ==============
eff_ptt <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
hdi_ptt_eff <- hdi(eff_ptt,ci=0.99)
hdi_ptt_eff<-hdi_ptt_eff%>%
  add_column(model='PTT',
             true=eff_true,
             manipulation=choice_set$manipulation[man_ind],
             choice=choice_set$choice[man_ind],
             trial_num=choice_set$num[man_ind],
             trial=choice_set$trial[man_ind])

## mhd =========
samples <- 
  readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/MHD_exp.rds'))

ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
### proportion ===========
hdi_mhd<-hdi(prop.1.Option,ci=0.99)
hdi_mhd<-hdi_mhd%>%
  add_column(model='MHD',
             true=m,
             manipulation=choice_set$manipulation,
             choice=choice_set$choice,
             trial_num=choice_set$num,
             trial=choice_set$trial)
### Effects ==============
eff_mhd <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
hdi_mhd_eff <- hdi(eff_mhd,ci=0.99)
hdi_mhd_eff<-hdi_mhd_eff%>%
  add_column(model='MHD',
             true=eff_true,
             manipulation=choice_set$manipulation[man_ind],
             choice=choice_set$choice[man_ind],
             trial_num=choice_set$num[man_ind],
             trial=choice_set$trial[man_ind])


data_prior <- rbind(hdi_hd,hdi_mhd,hdi_ptt)
eff_prior <- rbind(hdi_hd_eff,hdi_mhd_eff,hdi_ptt_eff)
saveRDS(data_prior,
        paste0('./RIC/output/results/prior_prediction/prior_',i,'/data_prior.rds'))
saveRDS(eff_prior,
        paste0('./RIC/output/results/prior_prediction/prior_',i,'/eff_prior.rds'))
