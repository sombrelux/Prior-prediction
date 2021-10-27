# fit pilot set, obtain prior 2
source('./RIC/src/requires.R')
rm(list=ls())
Sys.setenv(STAN_NUM_THREADS = 4)

pilot_choice <- 
  read_csv("./RIC/data/processed/pilot_choice.csv")
pilot_resp <- 
  read_csv("./RIC/data/processed/pilot_resp.csv")
k <- colSums(pilot_resp[,-1])
# HD ----------
data<-list(
  nTrial=nrow(pilot_choice),
  n=rep(20,nrow(pilot_choice)),
  N=20,
  x1=pilot_choice$x1,
  x2=pilot_choice$x2,
  t1=pilot_choice$t1,
  t2=pilot_choice$t2,
  o1=1/pilot_choice$p1-1,
  o2=1/pilot_choice$p2-1,
  k=k)

parameters <- c('a','logh','i','s','kpred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_HD.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 12)
saveRDS(samples,"./RIC/output/results/fit_pilot/HD_pilot.rds")

jpeg("./RIC/output/fig/fit_pilot/HD_trace.jpg")
traceplot(samples,pars=parameters[1:4])
dev.off()
jpeg("./RIC/output/fig/fit_pilot/HD_pair.jpg")
pairs(samples,pars=parameters[1:4])
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_pilot/HD_stats.csv')

# MHD ----------
parameters <- c('a','s','loghd','s_d','loghr','c','s_r','kpred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_MHD.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 12)
saveRDS(samples,"./RIC/output/results/fit_pilot/MHD_pilot.rds")

jpeg("./RIC/output/fig/fit_pilot/MHD_trace.jpg")
traceplot(samples,pars=parameters[1:7])
dev.off()
jpeg("./RIC/output/fig/fit_pilot/MHD_pair.jpg")
pairs(samples,pars=parameters[1:7])
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_pilot/MHD_stats.csv')

# PTT --------
data<-list(
  nTrial=nrow(pilot_choice),
  n=rep(20,nrow(pilot_choice)),
  N=20,
  x1=pilot_choice$x1,
  x2=pilot_choice$x2,
  t1=pilot_choice$t1,
  t2=pilot_choice$t2,
  p1=pilot_choice$p1,
  p2=pilot_choice$p2,
  k=k)
parameters <- c('alpha','beta','gamma','R','S','kpred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_PTT.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                control = list(adapt_delta = 0.9))
saveRDS(samples,"./RIC/output/results/fit_pilot/PTT_pilot.rds")

jpeg("./RIC/output/fig/fit_pilot/PTT_trace.jpg")
traceplot(samples,pars=parameters[1:5])
dev.off()
jpeg("./RIC/output/fig/fit_pilot/PTT_pair.jpg")
pairs(samples,pars=parameters[1:5])
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_pilot/PTT_stats.csv')

# RITCH ---------------
data<-list(
  nTrial=nrow(pilot_choice),
  n=rep(20,nrow(pilot_choice)),
  N=20,
  xs = sign(pilot_choice$x1 - pilot_choice$x2),
  ps = sign(pilot_choice$p1 - pilot_choice$p2),
  ts = sign(pilot_choice$t2 - pilot_choice$t1),
  xd = pilot_choice$x1 - pilot_choice$x2,
  xr = 2*(pilot_choice$x1 - pilot_choice$x2)/(pilot_choice$x1 + pilot_choice$x2),
  pd = pilot_choice$p1 - pilot_choice$p2,
  pr = 2*(pilot_choice$p1 - pilot_choice$p2)/(pilot_choice$p1 + pilot_choice$p2),
  td = pilot_choice$t2 - pilot_choice$t1,
  tr = 2*(pilot_choice$t2 - pilot_choice$t1)/(pilot_choice$t1 + pilot_choice$t2),
  k=k
)
data$tr[is.na(data$tr)] <- 0
parameters <- c(#'beta_rva','beta_dva',
  'beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr','kpred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_RITCH_p1.stan',
                data=data,
                pars=parameters,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123)
saveRDS(samples,"./RIC/output/results/fit_pilot/RITCH_pilot.rds")

jpeg("./RIC/output/fig/fit_pilot/RITCH_trace.jpg")
traceplot(samples,pars=parameters[1:9])
dev.off()
jpeg("./RIC/output/fig/fit_pilot/RITCH_pair.jpg")
pairs(samples,pars=parameters[1:9])
dev.off()

post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          './RIC/output/results/fit_pilot/RITCH_stats.csv')
