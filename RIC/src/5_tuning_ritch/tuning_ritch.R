source('./RIC/src/requires.R')
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
base_ind <- choice_set$manipulation=='Base'

i <- 4
if(!dir.exists('./RIC/output/results/tuning_ritch')){
  dir.create('./RIC/output/results/tuning_ritch')
}
if(!dir.exists('./RIC/output/fig/tuning_ritch')){
  dir.create('./RIC/output/fig/tuning_ritch')
}
# data prior ------
samples <- 
  readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/HD_exp.rds'))
ypred <- extract(samples)$ypred
kpred_hd <- apply(ypred,c(1,3),sum)

samples <- 
  readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/MHD_exp.rds'))
ypred <- extract(samples)$ypred
kpred_mhd <- apply(ypred,c(1,3),sum)

samples <- 
  readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/PTT_exp.rds'))
ypred <- extract(samples)$ypred
kpred_ptt <- apply(ypred,c(1,3),sum)

kpred_dp <- rbind(kpred_hd,kpred_mhd,kpred_ptt)[,base_ind]
dim(kpred_dp)
write_csv(as.data.frame(kpred_dp),'./RIC/output/results/tuning_ritch/kpred_dp.csv')

# Fit Ritch ---------
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')
base_ind <- choice_set$manipulation=='Base'
kpred_dp <- read_csv('./RIC/output/results/tuning_ritch/kpred_dp.csv')

x1 = choice_set$x1[base_ind]
x2 = choice_set$x2[base_ind]
t1 = choice_set$t1[base_ind]
t2 = choice_set$t2[base_ind]
p1 = choice_set$p1[base_ind]
p2 = choice_set$p2[base_ind]
xd=x1-x2;xs = sign(xd);xr=2*xd/(x1+x2)
td=t2-t1;ts = sign(td);tr=2*td/(t1+t2)
tr[is.na(tr)] <- 0
pd=p1-p2;ps = sign(pd);pr=2*pd/(p1+p2)

data <- list(
  nTrial = ncol(kpred_dp),
  n = 100,
  nsim = nrow(kpred_dp),
  xs=xs,ts=ts,ps=ps,
  xd=xd,td=td,pd=pd,
  xr=xr,tr=tr,pr=pr,
  k=kpred_dp
)

parameters <- c('beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr')

samples <- stan(file='./RIC/src/5_tuning_ritch/fit_ritch.stan',
                data=data,
                pars=parameters,
                iter=10,
                warmup = 0,
                chains=1, 
                thin=1,
                cores=1,
                seed = 123)
saveRDS(samples,
        "./RIC/output/results/tuning_ritch/RITCH.rds")

jpeg("./RIC/output/fig/tuning_ritch/RITCH_trace.jpg")
traceplot(samples,pars=parameters)
dev.off()
jpeg("./RIC/output/fig/tuning_ritch/RITCH_pairs.jpg")
pairs(samples,pars=parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/tuning_ritch/RITCH_stats.csv')
