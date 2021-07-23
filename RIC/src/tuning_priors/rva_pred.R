source('./RIC/src/requires.R')
rm(list=ls())

RvA_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice=='RvA')

# prior pred --------
## HD ------------
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
  file='./RIC/src/priors/prior_1_HD.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/prior_1/RvA/HD_1.rds")

## MHD --------------------
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
  file='./RIC/src/priors/prior_1_MHD.stan',
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/prior_1/RvA/MHD_1.rds")

## PTT -------------
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
  file='./RIC/src/priors/prior_1_PTT.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,"./RIC/output/results/prior_1/RvA/PTT_1.rds")

## RITCH ----------------
data<-list(
  nPart = 100,
  nTrial=nrow(RvA_set),
  rva_ind=rep(1,nrow(RvA_set)),
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
  file='./RIC/src/priors/prior_1_RITCH.stan',
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        "./RIC/output/results/prior_1/RvA/RITCH_1.rds")

# core pred ----------
## hd ===========
samples <- readRDS("./RIC/output/results/prior_1/RvA/HD_1.rds")
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_hd<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_hd%>%
  add_column(model='HD',
             m=apply(prop.1.Option,2,mean),
             manipulation=RvA_set$manipulation,
             trial_num=RvA_set$num,
             trial=RvA_set$trial)
## ptt =========
samples <- readRDS("./RIC/output/results/prior_1/RvA/PTT_1.rds")
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_ptt<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_ptt%>%
  add_column(model='PTT',
             m=apply(prop.1.Option,2,mean),
             manipulation=RvA_set$manipulation,
             trial_num=RvA_set$num,
             trial=RvA_set$trial)%>%
  bind_rows(hdi_model)
## mhd =========
samples <- readRDS("./RIC/output/results/prior_1/RvA/MHD_1.rds")
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_mhd<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_mhd%>%
  add_column(model='MHD',
             m=apply(prop.1.Option,2,mean),
             manipulation=RvA_set$manipulation,
             trial_num=RvA_set$num,
             trial=RvA_set$trial)%>%
  bind_rows(hdi_model)

## ritch ===========
samples <- readRDS("./RIC/output/results/prior_1/RvA/RITCH_1.rds")
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_ritch<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_ritch%>%
  add_column(model='RITCH',
             m=apply(prop.1.Option,2,mean),
             manipulation=RvA_set$manipulation,
             trial_num=RvA_set$num,
             trial=RvA_set$trial)%>%
  bind_rows(hdi_model)

saveRDS(hdi_model,'./RIC/output/results/prior_1/RvA/core.rds')

# plot ----------
hdi_model <- readRDS('./RIC/output/results/prior_1/RvA/core.rds')
hdi_model$model <- ifelse(hdi_model$model=='RITCH',
                          'RITCH','Data prior')
hdi_model_p <- hdi_model %>%
  group_by(model,manipulation,trial_num,trial)%>%
  summarise(lw=min(CI_low),
            up=max(CI_high))

  
ggplot(hdi_model_p, 
       mapping = aes(x = trial_num,group=model)) + 
  geom_ribbon(aes(ymin = lw, ymax = up,
                  fill=model), 
              alpha = 0.35) + 
  facet_wrap(~manipulation,nrow=2)+
  ylim(0,1)+
  labs(x = "Trial", 
       y = "Prop.Option.1 (Smaller Safer)",
       title = 'RvA')+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave('./RIC/output/fig/prior_1/RvA.svg',
       height = 4, width = 6)
