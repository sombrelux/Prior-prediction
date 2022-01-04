rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# choice set ----------------
choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))
## t2>=1, t1&t2<=72, x1,x2<=15000

df_sign <- choice_set%>%select(xs,ts,ps)
unique(df_sign) 
#xs    ts    ps
#-1     1     0
#-1     0     1
## beta_xo,beta_to,beta_po unidentifiable
## let beta_xt=beta_to-beta_xo, beta_xp=beta_po-beta_xo

## fit ==================
data<-list(
  nTrial = nrow(choice_set),
  xs = choice_set$xs,ts = choice_set$ts, ps = choice_set$ps,
  xd = choice_set$xd,td = choice_set$td, pd = choice_set$pd,
  xr = choice_set$xr,tr = choice_set$tr, pr = choice_set$pr,
  N = choice_set$N,y = choice_set$y)

parameters <- c('beta_xt','beta_xp',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr',
                'ypred')

samples <- stan(file = './RIC/src/3_fit_prev/fit_RITCH_choice.stan',
                data = data,
                pars = parameters,
                iter = 6000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                refresh = 100,
                control = list(max_treedepth = 15,
                               adapt_delta = 0.9))
saveRDS(samples, 
        './RIC/output/results/fit_prev/RITCH_group_choice.rds')

## post pred ==============
ypred <- extract(samples,pars='ypred')$ypred
dim(ypred)
hist(ypred[,2])
ypred <- as.data.frame(ypred)
hdi_ypred <- bayestestR::hdi(ypred,ci=0.99)
dim(hdi_ypred)
hdi_ypred[1:10,]
choice_set$y[1:10]

post_pred <- data.frame(y = choice_set$y,
                        CI_high = hdi_ypred$CI_high,
                        CI_low = hdi_ypred$CI_low)%>%
  add_column(x = 1:nrow(choice_set))

#post_pred1 <- post_pred[101:200,]
ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title='choice',x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/output/fig/fit_prev/RITCH_post_choice.png',
       height = 6,width = 18)

## post inference ============
png('./RIC/output/fig/fit_prev/RITCH_pairs_choice.png')
pairs(samples,pars = parameters[1:8])
dev.off()
png('./RIC/output/fig/fit_prev/RITCH_trace_choice.png')
traceplot(samples,pars = parameters[1:8])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:8,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/fit_prev/RITCH_param_choice.csv')

# indifference points ----------------
rm(list=ls())
vand15 <- read_csv("./RIC/data/previous/Vanderveldt15.csv")%>%
  rename(x1=Indifferences,x2=Amounts,t2=Delay,p2=Probability)%>%
  mutate(xd = x1-x2,td = t2, pd = 1-p2)%>%
  #filter(t2>0,p2<1)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2),
         pr = 2*pd/(1+p2))

df_sign <- vand15%>%select(xs,ts,ps)
unique(df_sign) 
#xs    ts    ps
#-1     1     1
#-1     1     0
#-1     0     1

## fit ==================
parameters <- c('beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr')
sigma = 0.01
data<-list(
    nTrial = nrow(vand15),
    xs = vand15$xs, ts = vand15$ts, ps = vand15$ps,
    xd = vand15$xd, xr = vand15$xr,
    td = vand15$td, tr = vand15$tr,
    pd = vand15$pd, pr = vand15$pr,
    sigma = sigma)
samples <- stan(file = './RIC/src/3_fit_prev/fit_RITCH_indiff.stan',
                  data = data,
                  pars = parameters,
                  iter = 13000,
                  warmup = 3000,
                  chains=4, 
                  thin=4,
                  cores=4,
                  seed = 123,
                  verbose = TRUE,
                  refresh = 100,
                  control = list(max_treedepth = 15,
                                 adapt_delta = 0.99))
saveRDS(samples,
        paste0('./RIC/output/results/fit_prev/RITCH_indiff_',
               sigma,'.rds'))

## post inference =============
png(paste0('./RIC/Output/fig/RITCH_pairs_indiff_',sigma,'.png'))
pairs(samples,pars = parameters[1:9])
dev.off()
png(paste0('./RIC/Output/fig/RITCH_trace_indiff_',sigma,'.png'))
traceplot(samples,pars = parameters[1:9])
dev.off()

post_param <- as.data.frame(summary(samples)$summary)%>%
  rownames_to_column()
post_param
write_csv(post_param,
          paste0('./RIC/output/results/fit_prev/RITCH_param_indiff_',sigma,'.csv'))

