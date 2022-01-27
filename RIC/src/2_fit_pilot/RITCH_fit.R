rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(HDInterval)

# choice set ----------------
choice_set <- read_csv("./RIC/data/processed/pilot_choice.csv")%>%
  dplyr::select(x1,p1,t1,x2,p2,t2)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))
resp_set <- read_csv("./RIC/data/processed/pilot_resp.csv")%>%
  dplyr::select(-ID)

df_sign <- choice_set%>%select(xs,ts,ps)
unique(df_sign) 
#xs    ts    ps
#-1     0     1
#-1     1     0
# 0     1    -1
## beta_xo,beta_to,beta_po unidentifiable
## let beta_xt=beta_to-beta_xo, beta_xp=beta_po-beta_xo

## fit ==================
data<-list(
  nTrial = nrow(choice_set),
  xs = choice_set$xs,ts = choice_set$ts, ps = choice_set$ps,
  xd = choice_set$xd,td = choice_set$td, pd = choice_set$pd,
  xr = choice_set$xr,tr = choice_set$tr, pr = choice_set$pr,
  N = 30,y = colSums(resp_set))

parameters <- c('beta_xt','beta_xp',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr',
                'ypred')

samples <- stan(file = './RIC/src/2_fit_pilot/fit_RITCH_choice.stan',
                data = data,
                pars = parameters,
                iter = 6000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                refresh = 100)
saveRDS(samples, './RIC/output/results/fit_pilot/RITCH.rds')

## post pred ==============
ypred <- extract(samples,pars='ypred')$ypred
ypred <- as.data.frame(ypred)
hdi_ypred <- hdi(ypred,credMass = 0.99)

post_pred <- data.frame(y = colSums(resp_set),
                        CI_high = hdi_ypred[2,],
                        CI_low = hdi_ypred[1,])%>%
  add_column(x = 1:nrow(choice_set))

ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title='RITCH',x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/output/fig/fit_pilot/RITCH_postpred.png',
       height = 6,width = 18)

## post inference ============
png('./RIC/output/fig/fit_pilot/RITCH_pairs.png')
pairs(samples,pars = parameters[1:8])
dev.off()
png('./RIC/output/fig/fit_pilot/RITCH_trace.png')
traceplot(samples,pars = parameters[1:8])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:8,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/fit_pilot/RITCH_postparam.csv')
