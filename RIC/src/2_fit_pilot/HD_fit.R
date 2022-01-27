rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(HDInterval)
dir.create('./RIC/output/results')
dir.create('./RIC/output/results/fit_pilot')
dir.create('./RIC/output/fig')
dir.create('./RIC/output/fig/fit_pilot')

# fit -------------------
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

parameters <- c('a','logh','i','s','ypred')
samples <- stan(file='./RIC/src/2_fit_pilot/fit_HD_choice.stan',
                data=data,
                pars=parameters,
                iter = 6000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                refresh = 100)
saveRDS(samples,'./RIC/output/results/fit_pilot/HD.rds')

# post pred ------------
post <- extract(samples)
ypred <- post$ypred
dim(ypred)
ypred <- as.data.frame(ypred)
hdi_ypred <- hdi(ypred,credMass = 0.99)

post_pred <- data.frame(y = colSums(resp_set),
                        CI_high = hdi_ypred[2,],
                        CI_low = hdi_ypred[1,])%>%
  add_column(x = 1:nrow(choice_set))

ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title='HD',x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/output/fig/fit_pilot/HD_postpred.png',
       height = 6,width = 18)

# post inference ------
png('./RIC/output/fig/fit_pilot/HD_pairs.png')
pairs(samples,pars = parameters[1:4])
dev.off()
png('./RIC/output/fig/fit_pilot/HD_trace.png')
traceplot(samples,pars = parameters[1:4])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:4,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/fit_pilot/HD_postparam.csv')
