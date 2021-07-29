#fit prior 1 based on indifference point
#obtain prior 2
source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)
Set <- 'GW'
indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")%>%
  filter((Probability!=1)|(Delay!=0),
         (exp==Set))
x1 <- indiff_set$Indifference
x2 <- indiff_set$Amounts
p2 <- indiff_set$Probability
t2 <- indiff_set$Delay

# Fit indiff set -----------
## HD ============
data<-list(
  nTrial=nrow(indiff_set),
  x1=x1,
  x2=x2, t2=t2, o2=1/p2-1,
  k=rep(0.5,nrow(indiff_set)))

parameters <- c('a','logh','i','s')
samples <- stan(file='./RIC/src/4_tuning_priors/fit_HD_indiff_2.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/tuning_priors/HD_indiff_",
               Set,".rds"))

jpeg(paste0("./RIC/output/fig/tuning_priors/HD_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters)
dev.off()
jpeg(paste0("./RIC/output/fig/tuning_priors/HD_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/tuning_priors/HD_',
                 Set,'_stats.csv'))
## MHD ===============
rm(list = c('data','samples','parameters'))
data<-list(
  nTrial=nrow(indiff_set),
  x1=x1,
  x2=x2, t2=t2, o2=1/p2-1,
  k=rep(0.5,nrow(indiff_set)))
parameters <- c('a','c','s',
                'loghd','loghr',
                's_d','s_r')
samples <- stan(file='./RIC/src/4_tuning_priors/fit_MHD_indiff_2.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/tuning_priors/MHD_indiff_",
               Set,".rds"))

jpeg(paste0("./RIC/output/fig/tuning_priors/MHD_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters)
dev.off()
jpeg(paste0("./RIC/output/fig/tuning_priors/MHD_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/tuning_priors/MHD_',
                 Set,'_stats.csv'))
## PTT ================
rm(list = c('data','samples','parameters'))
data<-list(
  nTrial=nrow(indiff_set),
  x1=x1,
  x2=x2, t2=t2, p2=p2,
  k=rep(0.5,nrow(indiff_set)))
parameters <- c('alpha','beta','gamma','R','S')
samples <- stan(file='./RIC/src/4_tuning_priors/fit_PTT_indiff_2.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/tuning_priors/PTT_indiff_",
               Set,".rds"))

jpeg(paste0("./RIC/output/fig/tuning_priors/PTT_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters)
dev.off()
jpeg(paste0("./RIC/output/fig/tuning_priors/PTT_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/tuning_priors/PTT_',
                 Set,'_stats.csv'))
## RITCH =============
rm(list = c('data','samples','parameters'))
data<-list(
  nTrial=nrow(indiff_set),
  xs = sign(x1-x2),ts=sign(t2),ps=sign(1-p2),
  xd = x1-x2,td=t2,pd=1-p2,
  xr = 2*(x1-x2)/(x1+x2),
  tr = 2*t2/t2,pr = 2*(1-p2)/(1+p2),
  k=rep(0.5,nrow(indiff_set)))
data$tr[is.na(data$tr)] <- 0
parameters <- c('beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr')

samples <- stan(file='./RIC/src/4_tuning_priors/fit_RITCH_indiff_2.stan',
                data=data,
                pars=parameters,
                iter=4000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,
        paste0("./RIC/output/results/tuning_priors/RITCH_indiff_",
               Set,".rds"))

jpeg(paste0("./RIC/output/fig/tuning_priors/RITCH_trace_",
            Set,".jpg"))
traceplot(samples,pars=parameters)
dev.off()
jpeg(paste0("./RIC/output/fig/tuning_priors/RITCH_pairs_",
            Set,".jpg"))
pairs(samples,pars=parameters)
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/tuning_priors/RITCH_',
                 Set,'_stats.csv'))
