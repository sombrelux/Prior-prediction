# make prior prediction for indifference point dataset
# tune parameters based on the results

source('./RIC/src/requires.R')
rm(list=ls())

indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")%>%
  filter((Probability!=1)|(Delay!=0))

x1 <- indiff_set$Indifference
p1 <- rep(1,nrow(indiff_set))
t1 <- rep(0,nrow(indiff_set))
x2 <- indiff_set$Amounts
p2 <- indiff_set$Probability
t2 <- indiff_set$Delay

i <- 1
if(!dir.exists(paste0('./RIC/output/results/tuning_priors/prior_',i))){
  dir.create(paste0('./RIC/output/results/tuning_priors/prior_',i))
}
if(!dir.exists(paste0('./RIC/output/fig/tuning_priors/prior_',i))){
  dir.create(paste0('./RIC/output/fig/tuning_priors/prior_',i))
}

# PRED INDIFF -------------
## HD -------------
data <- list(
  nPart = 100,
  nTrial=nrow(indiff_set),
  x1=x1,x2=x2,t1=t1,t2=t2,
  o1=1/p1-1,
  o2=1/p2-1)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_HD_',i,'.stan'),
  data=data,pars=parameters,iter = 500,
  warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/HD_indiff.rds'))

## PTT -------------
data <- list(
  nPart = 100,
  nTrial=nrow(indiff_set),
  x1=x1,
  x2=x2,
  t1=t1,
  t2=t2,
  p1=p1,
  p2=p2)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_PTT_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/PTT_indiff.rds'))

## MHD --------------------
data <- list(
  nPart = 100,
  nTrial=nrow(indiff_set),
  x1=x1,
  x2=x2,
  t1=t1,
  t2=t2,
  o1=1/p1-1,
  o2=1/p2-1)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_MHD_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/MHD_indiff.rds'))

## RITCH ----------------
data<-list(
  nPart = 100,
  nTrial=nrow(indiff_set),
  xs = sign(x1 - x2),
  ps = sign(p1 - p2),
  ts = sign(t2 - t1),
  xd = x1 - x2,
  xr = 2*(x1 - x2)/(x1 + x2),
  pd = p1 - p2,
  pr = 2*(p1 - p2)/(p1 + p2),
  td = t2 - t1,
  tr = 2*(t2 - t1)/(t1 + t2)
)
data$tr[is.na(data$tr)] <- 0
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_RITCH_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/RITCH_indiff.rds'))

# core ---------
## HD ---------
samples <- 
  readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/HD_indiff.rds'))
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)

hdi_hd <- hdi(prop.1.Option,ci = 0.99)

hdi_hd <- cbind(hdi_hd,x1,x2,p2,t2)
write_csv(hdi_hd,
          paste0('./RIC/output/results/tuning_priors/prior_',i,'/hd_indiff_hdi.csv'))

## mhd =============
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/MHD_indiff.rds'))

ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
hdi_mhd <- hdi(prop.1.Option,ci = 0.99)

hdi_mhd <- cbind(hdi_mhd,x1,x2,p2,t2)
write_csv(hdi_mhd,
          paste0('./RIC/output/results/tuning_priors/prior_',i,'/mhd_indiff_hdi.csv'))

## ptt ============
samples <- 
  readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/PTT_indiff.rds'))

ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
hdi_ptt <- hdi(prop.1.Option,ci = 0.99)

hdi_ptt <- cbind(hdi_ptt,x1,x2,p2,t2)
write_csv(hdi_ptt,
          paste0('./RIC/output/results/tuning_priors/prior_',i,'/ptt_indiff_hdi.csv'))

## ritch ========
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/RITCH_indiff.rds'))
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
hdi_ritch <- hdi(prop.1.Option,ci = 0.99)

hdi_ritch <- cbind(hdi_ritch,x1,x2,p2,t2)
write_csv(hdi_ritch,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/RITCH_indiff_hdi.csv'))
