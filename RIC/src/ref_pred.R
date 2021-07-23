source('./RIC/src/requires.R')
rm(list=ls())

ref_choice <- readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")
ref_interv <- data.frame(
  trial=1:nrow(ref_choice),
  lw = ref_choice$theta_lw,
  up = ref_choice$theta_up
)

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
  file='./RIC/src/data_prior/prior_1_HD.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/data_prior/prior_1/HD_1.rds")

## plots ---------
samples <- 
  readRDS("./RIC/output/results/data_prior/prior_1/HD_1.rds")
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
ggsave('./RIC/output/fig/data_prior/prior_1/HD.svg',
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
  file='./RIC/src/data_prior/prior_1_PTT.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/data_prior/prior_1/PTT_1.rds")

## plots ---------
samples <- readRDS("./RIC/output/results/data_prior/prior_1/PTT_1.rds")
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
ggsave('./RIC/output/fig/data_prior/prior_1/PTT.svg',
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
  file='./RIC/src/data_prior/prior_1_MHD.stan',
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/data_prior/prior_1/MHD_1.rds")

## plots ---------
samples <- readRDS("./RIC/output/results/data_prior/prior_1/MHD_1.rds")
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
ggsave('./RIC/output/fig/data_prior/prior_1/MHD.svg',
       width = 8,height = 4.75)

# RITCH ----------------
data<-list(
  nPart = 100,
  nTrial=nrow(ref_choice),
  rva_ind=ref_choice$p2<1,
  xd = ref_choice$x1 - ref_choice$x2,
  xr = 2*(ref_choice$x1 - ref_choice$x2)/(ref_choice$x1 + ref_choice$x2),
  pd = ref_choice$p1 - ref_choice$p2,
  pr = 2*(ref_choice$p1 - ref_choice$p2)/(ref_choice$p1 + ref_choice$p2),
  td = ref_choice$t1 - ref_choice$t2,
  tr = 2*(ref_choice$t1 - ref_choice$t2)/(ref_choice$t1 + ref_choice$t2)
)
data$tr[is.na(data$tr)] <- 0
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/RITCH/prior_1_RITCH.stan',
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        "./RIC/output/results/RITCH/prior_1.rds")
