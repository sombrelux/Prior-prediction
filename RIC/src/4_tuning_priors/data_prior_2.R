source('./RIC/src/requires.R')
rm(list=ls())

type <- 'DRvA'
ylabel <- "Prop.Option.1 (Smaller Safer Sooner)"
type_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice==type)

# prior pred --------
## HD ------------
data <- list(
  nPart = 100,
  nTrial=nrow(type_set),
  x1=type_set$x1,
  x2=type_set$x2,
  t1=type_set$t1,
  t2=type_set$t2,
  o1=1/type_set$p1-1,
  o2=1/type_set$p2-1)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/4_tuning_priors/priors/prior_HD_2.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_2/',
        type,'_HD.rds'))

## MHD --------------------
data <- list(
  nPart = 100,
  nTrial=nrow(type_set),
  x1=type_set$x1,
  x2=type_set$x2,
  t1=type_set$t1,
  t2=type_set$t2,
  o1=1/type_set$p1-1,
  o2=1/type_set$p2-1)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/4_tuning_priors/priors/prior_MHD_2.stan',
  data=data,pars=parameters,chains=4,
  iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_2/',
               type,'_MHD.rds'))
## PTT -------------
data <- list(
  nPart = 100,
  nTrial=nrow(type_set),
  x1=type_set$x1,
  x2=type_set$x2,
  t1=type_set$t1,
  t2=type_set$t2,
  p1=type_set$p1,
  p2=type_set$p2)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/4_tuning_priors/priors/prior_PTT_2.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
saveRDS(samples,
        paste0('./RIC/output/results/tuning_priors/prior_2/',
               type,'_PTT.rds'))
# core pred ----------
## hd ===========
samples <- 
  readRDS(paste0('./RIC/output/results/tuning_priors/prior_2/',
                 type,'_HD.rds'))
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_hd<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_hd%>%
  add_column(model='HD',
             m=apply(prop.1.Option,2,mean),
             manipulation=type_set$manipulation,
             trial_num=type_set$num,
             trial=type_set$trial)

## ptt =========
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_2/',
                          type,'_PTT.rds'))
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_ptt<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_ptt%>%
  add_column(model='PTT',
             m=apply(prop.1.Option,2,mean),
             manipulation=type_set$manipulation,
             trial_num=type_set$num,
             trial=type_set$trial)%>%
  bind_rows(hdi_model)

## mhd =========
samples <- readRDS(paste0('./RIC/output/results/tuning_priors/prior_2/',
                          type,'_MHD.rds'))
ypred <- extract(samples)$ypred
prop.1.Option<-data.frame(apply(ypred,c(1,3),mean))
hdi_mhd<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_mhd%>%
  add_column(model='MHD',
             m=apply(prop.1.Option,2,mean),
             manipulation=type_set$manipulation,
             trial_num=type_set$num,
             trial=type_set$trial)%>%
  bind_rows(hdi_model)

saveRDS(hdi_model,
        paste0('./RIC/output/results/tuning_priors/prior_2/',
        type,'_dtprior.rds'))

# plot ----------
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
       y = ylabel,
       title = type)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave(paste0('./RIC/output/fig/tuning_priors/prior_2/',
       type,'_dtprior.jpg'),
       height = 4, width = 6)
