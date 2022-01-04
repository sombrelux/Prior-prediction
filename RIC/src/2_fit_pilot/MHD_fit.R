rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# choice --------------------
choice_set <- read_csv("./RIC/data/processed/pilot_choice.csv")
resp_set <- read_csv("./RIC/data/processed/pilot_resp.csv")%>%
  dplyr::select(-ID)

data <- list(
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = 30,
  y = colSums(resp_set))

parameters <- c('a','c','logh_d','logh_r','s_d','s_r','s','ypred')
samples <- stan(file='./RIC/src/2_fit_pilot/fit_MHD_choice.stan',
                  data=data,
                  pars=parameters,
                  iter = 6000,
                  warmup = 2000,
                  chains= 4, 
                  thin=4,
                  cores=4,
                  seed = 123,
                  refresh = 100)
saveRDS(samples,'./RIC/output/results/fit_pilot/MHD.rds')

## post pred =============
ypred <- extract(samples,pars='ypred')$ypred
ypred <- as.data.frame(ypred)
hdi_ypred <- hdi(ypred,ci=0.99)
  
post_pred <- data.frame(y = colSums(resp_set),
                          CI_high = hdi_ypred$CI_high,
                          CI_low = hdi_ypred$CI_low)%>%
  add_column(x = 1:nrow(choice_set))
  
ggplot(post_pred,aes(x,y))+
    geom_point()+
    geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
    labs(title='MHD',x='Trial',y='# Option 1')+
    theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/output/fig/fit_pilot/MHD_postpred.png',
         height = 6,width = 18)

## post inference =============
png('./RIC/output/fig/fit_pilot/MHD_pairs.png')
pairs(samples,pars = parameters[1:7])
dev.off()
png('./RIC/output/fig/fit_pilot/MHD_trace.png')
traceplot(samples,pars = parameters[1:7])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:7,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/fit_pilot/MHD_postparam.csv')
