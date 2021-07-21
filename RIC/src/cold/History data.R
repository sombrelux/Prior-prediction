# History data ------------
source('./src/requires.R')
Sys.setenv(STAN_NUM_THREADS = 4)
## Risky ==================
rm(list=ls())
erev_set<-
  read_csv("./Data/history data/Erev2002.csv")
x1<-erev_set$v1
x2<-erev_set$v2
x1[erev_set$v1>erev_set$v2]<-
  erev_set$v2[erev_set$v1>erev_set$v2]
x2[erev_set$v1>erev_set$v2]<-
  erev_set$v1[erev_set$v1>erev_set$v2]
all(x1<=x2)

p1<-erev_set$p1
p2<-erev_set$p2
p1[erev_set$v1>erev_set$v2]<-
  erev_set$p2[erev_set$v1>erev_set$v2]
p2[erev_set$v1>erev_set$v2]<-
  erev_set$p1[erev_set$v1>erev_set$v2]
all(p1>=p2)

#prob to choose smaller safer
theta<-erev_set$o
theta[erev_set$v1>erev_set$v2]<-
  1-theta[erev_set$v1>erev_set$v2] 

k<-floor(38*theta)

### HD ========================
data<-list(
  nPart=38,
  nTrial=200,
  x1=x1,
  x2=x2,
  o1=1/p1-1,
  o2=1/p2-1,
  k=k
)

parameters <- c('a','h','s','kpred')
samples_hd <- stan(file='./src/Informative priors/Fit HistoryData/HD_risky.stan',
                   data=data,
                   pars=parameters,
                   iter= 2000, 
                   chains=4, 
                   thin=4,
                   cores=4,
                   warmup =  1000,
                   seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Risky_HD_pairs.jpeg')
pairs(samples_hd,pars=parameters[1:3])
dev.off()

traceplot(samples_hd,pars=parameters[1:3])

kpred<-data.frame(extract(samples_hd)$kpred)
dim(kpred)
hdi_hd<-hdi(kpred,ci=0.99)
dim(hdi_hd)
hdi_hd<-hdi_hd%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:200)
ggplot(as.data.frame(hdi_hd), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

hd_posteior <- as.array(samples_hd)
mcmc_dens_overlay(hd_posteior, 
                  pars = parameters[1:3])
ggsave('./Output/Informative priors/Fit HistoryData/Risky_HD_dens.jpeg')

hd_posteior_df<-as.data.frame(samples_hd)[,1:3]
dim(hd_posteior_df)
hd_summary <- summary(samples_hd)
hd_summ<-as.data.frame(hd_summary$summary)[1:3,]
hd_summ<-cbind(hd_summ,cor(hd_posteior_df))
hd_summ
write.csv(hd_summ,"./Output/Informative priors/Fit HistoryData/Risky_HD.csv")

### MHD ======================
data<-list(
  nPart=38,
  nTrial=200,
  logx1=log(x1),
  logx2=log(x2),
  o1=1/p1-1,
  o2=1/p2-1,
  k=k
)
parameters <- c('a','s','c','inv_hr','s_r','kpred')
samples_mhd <- stan(file='./src/Informative priors/Fit HistoryData/MHD_risky.stan',
                   data=data,
                   pars=parameters,
                   iter= 2000, 
                   chains=4, 
                   thin=4,
                   cores=4,
                   warmup = 1000,
                   seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Risky_MHD_pairs.jpeg')
pairs(samples_mhd,pars=parameters[1:5])
dev.off()

kpred<-data.frame(extract(samples_mhd)$kpred)
dim(kpred)
hdi_mhd<-hdi(kpred,ci=0.99)
dim(hdi_mhd)
hdi_mhd<-hdi_mhd%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:200)
ggplot(as.data.frame(hdi_mhd), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

mhd_posteior <- as.array(samples_mhd)
mcmc_dens_overlay(mhd_posteior, 
                  pars = parameters[1:5])
ggsave('./Output/Informative priors/Fit HistoryData/Risky_MHD_dens.jpeg')

mhd_posteior_df<-as.data.frame(samples_mhd)[,1:5]
mhd_summary <- summary(samples_mhd)
mhd_summ<-as.data.frame(mhd_summary$summary)[1:5,]
mhd_summ<-cbind(mhd_summ,cor(mhd_posteior_df))
mhd_summ
write.csv(mhd_summ,"./Output/Informative priors/Fit HistoryData/Risky_MHD.csv")

### PTT ==========================
data<-list(
  nPart=38,
  nTrial=200,
  x1=x1,
  x2=x2,
  p1=p1,
  p2=p2,
  k=k
)

parameters <- c('alpha','beta','gamma','s','kpred')
samples_ptt <- stan(file='./src/Informative priors/Fit HistoryData/PTT_risky.stan',
                    data=data,
                    pars=parameters,
                    iter= 2000, 
                    chains=4, 
                    thin=4,
                    cores=4,
                    warmup = 1000,
                    seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Risky_PTT_pairs.jpeg')
pairs(samples_ptt,pars=parameters[1:4])
dev.off()

kpred<-data.frame(extract(samples_ptt)$kpred)
dim(kpred)
hdi_ptt<-hdi(kpred,ci=0.99)
dim(hdi_ptt)
hdi_ptt<-hdi_ptt%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:200)
ggplot(as.data.frame(hdi_ptt), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

ptt_posteior <- as.array(samples_ptt)
mcmc_dens_overlay(ptt_posteior, 
                  pars = parameters[1:4])
ggsave('./Output/Informative priors/Fit HistoryData/Risky_PTT.jpeg')

ptt_posteior_df<-as.data.frame(samples_ptt)[,1:4]
ptt_summary <- summary(samples_ptt)
ptt_summ<-as.data.frame(ptt_summary$summary)[1:4,]
ptt_summ<-cbind(ptt_summ,cor(ptt_posteior_df))
ptt_summ
write.csv(ptt_summ,"./Output/Informative priors/Fit HistoryData/Risky_PTT.csv")

### RITCH ========================
xd=x1-x2
pd=p1-p2

xs=sign(xd)
ps=sign(pd)

xr=xd/(x1+x2)*2
pr=pd/(p1+p2)*2

data<-list(
  nPart=38,
  nTrial=200,
  xs=xs,
  ps=ps,
  xd=xd,
  pd=pd,
  xr=xr,
  pr=pr,
  k=k
)

parameters <- c('beta_o','beta_xa','beta_xr','beta_pa','beta_pr','kpred')
samples_ritch <- stan(file='./src/Informative priors/Fit HistoryData/RITCH_risky.stan',
                      data=data,
                      pars=parameters,
                      iter= 2000, 
                      chains=4, 
                      thin=4,
                      cores=4,
                      warmup = 1000,
                      seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Risky_RITCH_pairs.jpeg')
pairs(samples_ritch,pars=parameters[1:5])
dev.off()

kpred<-data.frame(extract(samples_ritch)$kpred)
dim(kpred)
hdi_ritch<-hdi(kpred,ci=0.99)
dim(hdi_ritch)
hdi_ritch<-hdi_ritch%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:200)
ggplot(as.data.frame(hdi_ritch), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

ritch_posteior <- as.array(samples_ritch)
mcmc_dens_overlay(ritch_posteior, 
                  pars = parameters[1:5])
ggsave('./Output/Informative priors/Fit HistoryData/Risky_RITCH.jpeg')

ritch_posteior_df<-as.data.frame(samples_ritch)[,1:5]
ritch_summary <- summary(samples_ritch)
ritch_summ<-as.data.frame(ritch_summary$summary)[1:5,]
ritch_summ<-cbind(ritch_summ,cor(ritch_posteior_df))
ritch_summ
write.csv(ritch_summ,"./Output/Informative priors/Fit HistoryData/Risky_RITCH.csv")

## Intertemporal ==================
rm(list=ls())
itch_set<-read_csv("./Data/history data/Ericson_et_al_2015.csv")
intertemp_set<-itch_set%>%
  filter(Condition==3,
         !is.na(LaterOptionChosen))%>%
  mutate(y=dplyr::recode(LaterOptionChosen,
                         '1'=0,'0'=1))
group_id<-intertemp_set%>%
  group_by(Subject)%>%
  group_indices()

question_id<-intertemp_set%>%
  group_by(X1,X2,T1,T2)%>%
  group_indices()

intertemp_set<-intertemp_set%>%
  add_column(ID=group_id,Choice=question_id)

group_result<-intertemp_set%>%
  group_by(Choice)%>%
  summarise(x1=mean(X1),x2=mean(X2),
            t1=mean(T1)/4,t2=mean(T2)/4,
            n=n(),k=sum(y),
            true=mean(y))%>%
  select(x1,t1,x2,t2,n,k,true)

x1<-group_result$x1
x2<-group_result$x2
t1<-group_result$t1
t2<-group_result$t2
n<-group_result$n
k<-group_result$k
nTrial<-nrow(group_result)
### HD ======================
data<-list(
  nTrial=nTrial,
  x1=x1,
  x2=x2,
  o1=rep(0,nTrial),
  o2=rep(0,nTrial),
  t1=t1,
  t2=t2,
  n=n,
  k=k,
  mu=c(0.3,0.01,0.8,1.5),
  sigma = c(0.015,1,0.05,0.1),
  R=matrix(c(1,0,0.71,-0.93,
             0,1,0,0,
             0.71,0,1,-0.65,
             -0.93,0,-0.65,1),nrow=4,byrow=T)
)

parameters <- c('beta','kpred')#c('a','h','s')#,
                #'sig_a','sig_h','sig_s')
samples_hd <- stan(file='./src/Informative priors/Fit HistoryData/HD_mu.stan',
                   data=data,
                   pars=parameters,
                   iter= 2000, 
                   chains=4, 
                   thin=4,
                   cores=4,
                   warmup =  1000,
                   seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Temp_HD_pairs.jpeg')
pairs(samples_hd,pars=parameters[1])
dev.off()

kpred<-data.frame(extract(samples_hd)$kpred)
dim(kpred)
hdi_hd<-hdi(kpred,ci=0.99)
dim(hdi_hd)
hdi_hd<-hdi_hd%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:nTrial)
ggplot(as.data.frame(hdi_hd), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

hd_posteior <- as.array(samples_hd)
mcmc_dens_overlay(hd_posteior, 
                  pars = c('beta[1]','beta[2]','beta[3]','beta[4]'))
ggsave('./Output/Informative priors/Fit HistoryData/Temp_HD.jpg')

hd_posteior_df<-as.data.frame(samples_hd)[,1:4]
dim(hd_posteior_df)
hd_summary <- summary(samples_hd)
hd_summ<-as.data.frame(hd_summary$summary)[1:4,]
hd_summ<-cbind(hd_summ,cor(hd_posteior_df))
hd_summ
write.csv(hd_summ,"./Output/Informative priors/Fit HistoryData/Temp_HD.csv")

### MHD ===============
data<-list(
  nTrial=nTrial,
  logx1=log(x1),
  logx2=log(x2),
  t1=t1,
  t2=t2,
  n=n,
  k=k,
  #mu=c(0.2,1,1,3.7),
  #sigma=c(0.05,1,1,1),
  #R=matrix(c(1,0,0,-0.96,
   #          0,1,0,0,
    #         0,0,1,0,
     #        -0.96,0,0,1),nrow=4,
      #     byrow=T)
)

parameters <- c('a','s','inv_hd','s_d','kpred')#c('beta','kpred')
samples_mhd <- stan(file='./src/Informative priors/Fit HistoryData/MHD_temp.stan',
                   data=data,
                   pars=parameters,
                   iter= 2000, 
                   chains=4, 
                   thin=4,
                   cores=4,
                   warmup =  1000,
                   seed = 123)#,
                  # control = list(max_treedepth = 16,
                 #                 adapt_delta = 0.9))

print(get_elapsed_time(samples_mhd))

inits <- get_inits(samples_mhd)
print(inits[[1]][1])
print(inits[[2]][1])
print(inits[[3]][1])
print(inits[[4]][1])

traceplot(samples_mhd,pars=parameters[1:4])

jpeg('./Output/Informative priors/Fit HistoryData/Temp_MHD_pairs.jpeg')
pairs(samples_mhd,pars=parameters[1:4])
dev.off()

kpred<-data.frame(extract(samples_mhd)$kpred)
dim(kpred)
hdi_mhd<-hdi(kpred,ci=0.99)
dim(hdi_mhd)
hdi_mhd<-hdi_mhd%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:nTrial)
ggplot(as.data.frame(hdi_mhd), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

mhd_posteior <- as.array(samples_mhd)
mcmc_dens_overlay(mhd_posteior, 
                  pars = c('a','s','inv_hd','s_d'))
ggsave('./Output/Informative priors/Fit HistoryData/Temp_MHD.jpg')

mhd_posteior_df<-as.data.frame(samples_mhd)[,1:4]
dim(mhd_posteior_df)
mhd_summary <- summary(samples_mhd)
mhd_summ<-as.data.frame(mhd_summary$summary)[1:4,]
mhd_summ<-cbind(mhd_summ,cor(mhd_posteior_df))
mhd_summ
write.csv(mhd_summ,"./Output/Informative priors/Fit HistoryData/Temp_MHD.csv")

### PTT ===============
data<-list(
  nTrial=nTrial,
  x1=x1,
  x2=x2,
  p1=rep(1,nTrial),
  p2=rep(1,nTrial),
  t1=t1,
  t2=t2,
  n=n,
  k=k,
  mu=c(0.015,0.65,1,0.75,1.8),
  sigma=c(0.01,0.01,1,0.03,0.11),
  R=matrix(c(1,-0.54,0,0.016,-0.026,
             -0.54,1,0,-0.3,0.7,
             0,0,1,0,0,
             0.016,-0.3,0,1,-0.68,
             -0.026,0.7,0,-0.68,1),
           nrow = 5,byrow = T)
)

parameters <- c('beta','kpred')
samples_ptt <- stan(file='./src/Informative priors/Fit HistoryData/PTT_mu.stan',
                   data=data,
                   pars=parameters,
                   iter= 2000, 
                   chains=4, 
                   thin=4,
                   cores=4,
                   warmup =  1000,
                   seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Temp_PTT_pairs.jpeg')
pairs(samples_ptt,pars=parameters[1])
dev.off()

kpred<-data.frame(extract(samples_ptt)$kpred)
dim(kpred)
hdi_ptt<-hdi(kpred,ci=0.99)
dim(hdi_ptt)
hdi_ptt<-hdi_ptt%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:nTrial)
ggplot(as.data.frame(hdi_ptt), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

ptt_posteior <- as.array(samples_ptt)
mcmc_dens_overlay(ptt_posteior, 
                  pars = c('beta[1]','beta[2]','beta[3]','beta[4]',
                           'beta[5]'))
ggsave('./Output/Informative priors/Fit HistoryData/Temp_PTT.jpg')

ptt_posteior_df<-as.data.frame(samples_ptt)[,1:5]
dim(ptt_posteior_df)
ptt_summary <- summary(samples_ptt)
ptt_summ<-as.data.frame(ptt_summary$summary)[1:5,]
ptt_summ<-cbind(ptt_summ,cor(ptt_posteior_df))
ptt_summ
write.csv(ptt_summ,"./Output/Informative priors/Fit HistoryData/Temp_PTT.csv")

### RITCH ==================
xd=x1-x2
td=t1-t2

xs=sign(xd)
ts=sign(td)

xr=xd/(x1+x2)*2
tr=td/(t1+t2)*2

data<-list(
  nTrial=nTrial,
  xs=xs,
  ts=ts,
  xd=xd,
  td=td,
  xr=xr,
  tr=tr,
  n=n,
  k=k,
  mu=c(0.01,1.4,1,1),
  sigma=c(0.0025,0.11,1,1),
  R=matrix(c(1,-0.84,0,0,
             -0.84,1,0,0,
             0,0,1,0,
             0,0,0,1),nrow=4,byrow = T)
)

parameters <- c('beta_o','beta','kpred')
samples_ritch <- stan(file='./src/Informative priors/Fit HistoryData/RITCH_mu.stan',
                      data=data,
                      pars=parameters,
                      iter= 2000, 
                      chains=4, 
                      thin=4,
                      cores=4,
                      warmup = 1000,
                      seed = 123)

jpeg('./Output/Informative priors/Fit HistoryData/Temp_RITCH_pairs.jpeg')
pairs(samples_ritch,pars=parameters[1:2])
dev.off()

kpred<-data.frame(extract(samples_ritch)$kpred)
dim(kpred)
hdi_ritch<-hdi(kpred,ci=0.99)
dim(hdi_ritch)
hdi_ritch<-hdi_ritch%>%
  add_column(true=k)%>%
  arrange(true)%>%
  add_column(trial=1:nTrial)
ggplot(as.data.frame(hdi_ritch), 
       mapping = aes(x = trial)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))

ritch_posteior <- as.array(samples_ritch)
mcmc_dens_overlay(ritch_posteior, 
                  pars = c('beta_o','beta[1]','beta[2]',
                           'beta[3]','beta[4]'))
ggsave('./Output/Informative priors/Fit HistoryData/Temp_RITCH.jpg')

ritch_posteior_df<-as.data.frame(samples_ritch)[,1:5]
dim(ritch_posteior_df)
ritch_summary <- summary(samples_ritch)
ritch_summ<-as.data.frame(ritch_summary$summary)[1:5,]
ritch_summ<-cbind(ritch_summ,cor(ritch_posteior_df))
ritch_summ
write.csv(ritch_summ,"./Output/Informative priors/Fit HistoryData/Temp_RITCH.csv")
