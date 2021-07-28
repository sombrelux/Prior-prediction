source('./RIC/src/requires.R')
rm(list=ls())

ref_choice <- readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")
ref_interv <- data.frame(
  trial=1:nrow(ref_choice),
  lw = ref_choice$theta_lw,
  up = ref_choice$theta_up
)

i <- 3
if(!dir.exists(paste0('./RIC/output/results/tuning_priors/prior_',i))){
  dir.create(paste0('./RIC/output/results/tuning_priors/prior_',i))
}
if(!dir.exists(paste0('./RIC/output/fig/tuning_priors/prior_',i))){
  dir.create(paste0('./RIC/output/fig/tuning_priors/prior_',i))
}

# HD -------------
data <- list(
  nPart = 100,
  nTrial=nrow(ref_choice),
  x1=ref_choice$x1,
  x2=ref_choice$x2,
  t1=ref_choice$t1,
  t2=ref_choice$t2,
  o1=1/ref_choice$p1-1,
  o2=1/ref_choice$p2-1)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_HD_',i,'.stan'),
  data=data,pars=parameters,iter = 500,
  warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_HD.rds'))

## plots ---------
samples <- 
  readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_HD.rds'))
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(ref_choice)

hd_prop<-prop.1.Option%>%
  pivot_longer(everything(),names_to = 'trial',
               values_to='prop')%>%
  mutate(trial = as.numeric(trial))

ggplot(ref_interv,aes(x=trial))+
  geom_segment(aes(xend=trial,y=lw,yend=up))+
  geom_ribbon(aes(ymin=lw,ymax=up),alpha=0.5,
              fill='lightskyblue2')+
  geom_point(aes(y=prop),data=hd_prop,alpha=0.5)+
  scale_x_continuous('Trial',
                     breaks=1:32,labels=1:32)+
  labs(title = 'HD',y='Proportion to chose option 1')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0('./RIC/output/fig/tuning_priors/prior_',i,'/ref_HD.jpg'),
       width = 8,height = 4.75)

# PTT -------------
data <- list(
  nPart = 100,
  nTrial=nrow(ref_choice),
  x1=ref_choice$x1,
  x2=ref_choice$x2,
  t1=ref_choice$t1,
  t2=ref_choice$t2,
  p1=ref_choice$p1,
  p2=ref_choice$p2)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_PTT_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_PTT.rds'))

## plots ---------
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_PTT.rds'))

ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(ref_choice)

ptt_prop<-prop.1.Option%>%
  pivot_longer(everything(),names_to = 'trial',
               values_to='prop')%>%
  mutate(trial = as.numeric(trial))

ggplot(ref_interv,aes(x=trial))+
  geom_segment(aes(xend=trial,y=lw,yend=up))+
  geom_ribbon(aes(ymin=lw,ymax=up),alpha=0.5,
              fill='lightskyblue2')+
  geom_point(aes(y=prop),data=ptt_prop,alpha=0.5)+
  scale_x_continuous('Trial',
                     breaks=1:32,labels=1:32)+
  labs(title = 'PTT',y='Proportion to chose option 1')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0('./RIC/output/fig/tuning_priors/prior_',i,'/ref_PTT.jpg'),
       width = 8,height = 4.75)

# MHD --------------------
data <- list(
  nPart = 100,
  nTrial=nrow(ref_choice),
  x1=ref_choice$x1,
  x2=ref_choice$x2,
  t1=ref_choice$t1,
  t2=ref_choice$t2,
  o1=1/ref_choice$p1-1,
  o2=1/ref_choice$p2-1)
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_MHD_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_MHD.rds'))

## plots ---------
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_MHD.rds'))

ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(ref_choice)

mhd_prop<-prop.1.Option%>%
  pivot_longer(everything(),names_to = 'trial',
               values_to='prop')%>%
  mutate(trial = as.numeric(trial))

ggplot(ref_interv,aes(x=trial))+
  geom_segment(aes(xend=trial,y=lw,yend=up))+
  geom_ribbon(aes(ymin=lw,ymax=up),alpha=0.5,
              fill='lightskyblue2')+
  geom_point(aes(y=prop),data=mhd_prop,alpha=0.5)+
  scale_x_continuous('Trial',
                     breaks=1:32,labels=1:32)+
  labs(title = 'MHD',y='Proportion to chose option 1')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0('./RIC/output/fig/tuning_priors/prior_',i,'/ref_MHD.jpg'),
       width = 8,height = 4.75)

# RITCH ----------------
data<-list(
  nPart = 100,
  nTrial=nrow(ref_choice),
  xs = sign(ref_choice$x1 - ref_choice$x2),
  ps = sign(ref_choice$p1 - ref_choice$p2),
  ts = sign(ref_choice$t2 - ref_choice$t1),
  xd = ref_choice$x1 - ref_choice$x2,
  xr = 2*(ref_choice$x1 - ref_choice$x2)/(ref_choice$x1 + ref_choice$x2),
  pd = ref_choice$p1 - ref_choice$p2,
  pr = 2*(ref_choice$p1 - ref_choice$p2)/(ref_choice$p1 + ref_choice$p2),
  td = ref_choice$t2 - ref_choice$t1,
  tr = 2*(ref_choice$t2 - ref_choice$t1)/(ref_choice$t1 + ref_choice$t2)
)
data$tr[is.na(data$tr)] <- 0
parameters <- 'ypred'
samples <- stan(
  file=paste0('./RIC/src/4_tuning_priors/priors/prior_RITCH_',i,'.stan'),
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_RITCH.rds'))

## plots ---------
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_',i,'/ref_RITCH.rds'))
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(ref_choice)

ritch_prop<-prop.1.Option%>%
  pivot_longer(everything(),names_to = 'trial',
               values_to='prop')%>%
  mutate(trial = as.numeric(trial))

ggplot(ref_interv,aes(x=trial))+
  geom_segment(aes(xend=trial,y=lw,yend=up))+
  geom_ribbon(aes(ymin=lw,ymax=up),alpha=0.5,
              fill='lightskyblue2')+
  geom_point(aes(y=prop),
             data=ritch_prop,alpha=0.5)+
  scale_x_continuous('Trial',
                     breaks=1:32,labels=1:32)+
  labs(y='Proportion to chose option 1')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0('./RIC/output/fig/tuning_priors/prior_',i,'/ref_RITCH.jpg'),
       width = 8,height = 4.75)

## inspect ritch ==========
beta_dva <- 1.1
beta_xa <- 0.0015
beta_xr <- 2
beta_ta <- 1.4
beta_tr <- 0.3

prev_df<-
  readRDS("./RIC/data/processed/prev_df.rds")%>%
  filter(x1<10000,x2<10000,p2==1)
xd = prev_df$x1 - prev_df$x2
range(xd) #-1000 -0.1
xr = 2*(prev_df$x1 - prev_df$x2)/(prev_df$x1 + prev_df$x2)
range(xr) #-1.2 -0.2
td = prev_df$t2 - prev_df$t1
range(td) #0.25 0.75
tr = 2*(prev_df$t2 - prev_df$t1)/(prev_df$t1 + prev_df$t2)
range(tr) #0.4 2

theta_logit_prev <- beta_dva+beta_xa*xd+
  beta_xr*xr+beta_ta*td+beta_tr*tr

intertemp_ref <- 
  ref_choice%>%filter(choice=='DvA')
xd = intertemp_ref$x1 - intertemp_ref$x2
range(xd) #-225 -75
xr = 2*(intertemp_ref$x1 - intertemp_ref$x2)/(intertemp_ref$x1 + intertemp_ref$x2)
range(xr) #-1.2 -0.19
td = intertemp_ref$t2 - intertemp_ref$t1
range(td) #4 29
tr = 2*(intertemp_ref$t2 - intertemp_ref$t1)/(intertemp_ref$t1 + intertemp_ref$t2)
range(tr) #0.25 1.9

theta_logit_ref <- beta_dva+beta_xa*xd+
  beta_xr*xr+beta_ta*td+beta_tr*tr

range(theta_logit_prev) #-2 2
range(theta_logit_ref) #5 40
