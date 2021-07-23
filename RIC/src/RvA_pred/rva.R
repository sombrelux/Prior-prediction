source('./RIC/src/requires.R')
rm(list=ls())

RvA_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice=='RvA')

# HD ------------
data <- list(
  nPart = 100,
  nTrial=nrow(RvA_set),
  x1=RvA_set$x1,
  x2=RvA_set$x2,
  t1=RvA_set$t1,
  t2=RvA_set$t2,
  o1=1/RvA_set$p1-1,
  o2=1/RvA_set$p2-1)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/data_prior/prior_1_HD.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/RvA/prior_1/HD_1.rds")
## plot ===========
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
hdi_model<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_model%>%
  add_column(model='HD',
             m=apply(prop.1.Option,2,mean),
             manipulation=RvA_set$manipulation,
             trial=RvA_set$num)

ggplot(as.data.frame(hdi_model), 
       mapping = aes(x = trial,group=model)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high,
                  fill=model), 
              alpha = 0.35) + 
  geom_line(mapping = aes(x = trial, y = m)) + 
  facet_wrap(~manipulation,nrow=2)+
  ylim(0,1)+
  labs(x = "Trial", y = "Prop.Option.1 (Smaller Safer)")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave('./RIC/output/fig/RvA_set/prior_1/HD.svg',
       width = 8,height = 4.75)
# PTT -------------
data <- list(
  nPart = 100,
  nTrial=nrow(RvA_set),
  x1=RvA_set$x1,
  x2=RvA_set$x2,
  t1=RvA_set$t1,
  t2=RvA_set$t2,
  p1=RvA_set$p1,
  p2=RvA_set$p2)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/data_prior/prior_1_PTT.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/RvA/prior_1/PTT_1.rds")
## plots ---------
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(RvA_set)

hdi_model<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_model%>%
  add_column(model='PTT',
             m=apply(prop.1.Option,2,mean),
             manipulation=RvA_set$manipulation,
             trial=RvA_set$num)

ggplot(as.data.frame(hdi_model), 
       mapping = aes(x = trial,group=model)) + 
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high,
                  fill=model), 
              alpha = 0.35) + 
  geom_line(mapping = aes(x = trial, y = m)) + 
  facet_wrap(~manipulation,nrow=2)+
  ylim(0,1)+
  labs(x = "Trial", y = "Prop.Option.1 (Smaller Safer)")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave('./RIC/output/fig/RvA_set/prior_1/PTT.svg',
       width = 8,height = 4.75)
# MHD --------------------

data <- list(
  nPart = 100,
  nTrial=nrow(RvA_set),
  x1=RvA_set$x1,
  x2=RvA_set$x2,
  t1=RvA_set$t1,
  t2=RvA_set$t2,
  o1=1/RvA_set$p1-1,
  o2=1/RvA_set$p2-1)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/RvA_set/prior_1_MHD.stan',
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/RvA_set/prior_1/MHD_1.rds")

## plots ---------
samples <- readRDS("./RIC/output/results/RvA_set/prior_1/MHD_1.rds")
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(RvA_set)

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
ggsave('./RIC/output/fig/RvA_set/prior_1/MHD.svg',
       width = 8,height = 4.75)

# RITCH ----------------
data<-list(
  nPart = 100,
  nTrial=nrow(RvA_set),
  rva_ind=RvA_set$p2<1,
  xd = RvA_set$x1 - RvA_set$x2,
  xr = 2*(RvA_set$x1 - RvA_set$x2)/(RvA_set$x1 + RvA_set$x2),
  pd = RvA_set$p1 - RvA_set$p2,
  pr = 2*(RvA_set$p1 - RvA_set$p2)/(RvA_set$p1 + RvA_set$p2),
  td = RvA_set$t1 - RvA_set$t2,
  tr = 2*(RvA_set$t1 - RvA_set$t2)/(RvA_set$t1 + RvA_set$t2)
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

## plots ---------
samples <- readRDS("./RIC/output/results//RITCH/prior_1.rds")
ypred <- extract(samples)$ypred
dim(ypred)
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
dim(prop.1.Option)
colnames(prop.1.Option) <- 1:nrow(RvA_set)

ritch_prop<-prop.1.Option%>%
  pivot_longer(everything(),names_to = 'trial',
               values_to='prop')%>%
  mutate(trial = as.numeric(trial))

ggplot(ref_interv,aes(x=trial))+
  geom_segment(aes(xend=trial,y=lw,yend=up))+
  geom_ribbon(aes(ymin=lw,ymax=up),alpha=0.5,
              fill='lightskyblue2')+
  geom_point(aes(y=prop),data=ritch_prop,alpha=0.5)+
  scale_x_continuous('Trial',
                     breaks=1:32,labels=1:32)+
  labs(title = 'RITCH',y='Proportion to chose option 1')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave('./RIC/output/fig/RITCH/prior_1.svg',
       width = 8,height = 4.75)
