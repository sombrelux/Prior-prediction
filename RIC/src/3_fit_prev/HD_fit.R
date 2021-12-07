rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

# choice -------------------
choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)
## t2>=1, t1&t2<=72, x1,x2<=15000

data <- list(
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = choice_set$N,
  y = choice_set$y)

parameters <- c('a','logh','i','s','ypred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_HD_choice.stan',
                data=data,
                pars=parameters,
                iter = 13000,
                warmup = 3000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
saveRDS(samples,
        './RIC/output/results/fit_prev/HD_group_choice.rds')

## post pred ===========
ypred <- extract(samples,pars='ypred')$ypred
dim(ypred)
ypred <- as.data.frame(ypred)
hdi_ypred <- bayestestR::hdi(ypred,ci=0.99)
dim(hdi_ypred)

post_pred <- data.frame(y = choice_set$y,
                        CI_high = hdi_ypred$CI_high,
                        CI_low = hdi_ypred$CI_low)%>%
  add_column(x = 1:nrow(choice_set))

ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title='Group',x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/Output/fig/fit_prev/HD_post_group_choice.png',
       height = 6,width = 18)

## post inference =============
png('./RIC/Output/fig/fit_prev/HD_pairs_group_choice.png')
pairs(samples,pars = parameters[1:4])
dev.off()
png('./RIC/Output/fig/fit_prev/HD_trace_group_choice.png')
traceplot(samples,pars = parameters[1:4])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:4,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/fit_prev/HD_param_choice.csv')
