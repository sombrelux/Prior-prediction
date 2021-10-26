rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
#Sys.setenv(STAN_NUM_THREADS = 4)
library(bayestestR)

choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)
## t2>=1, t1&t2<=72, x1,x2<=15000

# group ---------------
data <- list(
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = choice_set$N,
  y = choice_set$y)
parameters <- c('a','c','logh_d','logh_r',
                'logs_d','logs_r','s','ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_MHD_ind.stan',
                data=data,
                pars=parameters,
                iter = 2000,
                warmup = 1000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
saveRDS(samples,
        './RIC/output/results/data_prior/MHD_group.rds')

## post pred =============

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
  labs(title='Group',x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/Output/fig/fit_prev/MHD/post_group.png',
       height = 6,width = 18)

## post inference ============
png('./RIC/Output/fig/fit_prev/MHD/pairs_group.png')
pairs(samples,pars = parameters[1:7])
dev.off()
png('./RIC/Output/fig/fit_prev/MHD/trace_group.png')
traceplot(samples,pars = parameters[1:7])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:7,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/data_prior/MHD_param.csv')
