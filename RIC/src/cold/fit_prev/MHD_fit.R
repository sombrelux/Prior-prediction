rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)
#Sys.setenv(STAN_NUM_THREADS = 6)

choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)
## t2>=1, t1&t2<=72, x1,x2<=15000

Exp_list <- unique(choice_all$Exp)
#Exp <- NULL
#for(i in 1:nrow(choice_set))  Exp[i] <- which(choice_set$Exp[i]==Exp_list)

# fit mhd choice --------------------
parameters <- c('a','c','logh_d','logh_r','s_r','s_d','s',
                #'sig','a_i','c_i','loghd_i','loghr_i','sr_i','sd_i','s_i',
                'ypred')

#for(i in 6:17){
#1:6,9,10,:2000, 6000
#7: 4000, 8000
#8,10,12
#i=13
 # choice_set <- choice_all%>%filter(Exp==Exp_list[i])
  data <- list(
    #nExp = length(unique(choice_set$Exp)),
    nTrial = nrow(choice_set),
    #Exp = Exp,
    x1 = choice_set$x1, x2 = choice_set$x2,
    t1 = choice_set$t1, t2 = choice_set$t2,
    o1 = 1/choice_set$p1-1,
    o2 = 1/choice_set$p2-1,
    N = choice_set$N,
    y = choice_set$y)
  
  samples <- stan(file='./RIC/src/3_fit_prev/fit_MHD_choice.stan',
                  data=data,
                  pars=parameters,
                  iter = 6000,#10000,
                  warmup = 2000,
                  chains= 4, 
                  thin=4,
                  cores=4,
                  seed = 123,
                  refresh = 100,
                  control = list(max_treedepth = 15#, adapt_delta = 0.99
                  ))
  saveRDS(samples,'./RIC/output/results/fit_prev/MHD_group_choice.rds')
  
#}
## post pred =============
#rm(list=ls())
#for(i in 1:17){
  #choice_set <- choice_all%>%filter(Exp==Exp_list[i])
  #samples <- readRDS(paste0('./RIC/output/results/fit_prev/MHD_exp_',i,'.rds'))
  ypred <- extract(samples,pars='ypred')$ypred
  ypred <- as.data.frame(ypred)
  hdi_ypred <- bayestestR::hdi(ypred,ci=0.99)
  
  post_pred <- data.frame(y = choice_set$y,
                          CI_high = hdi_ypred$CI_high,
                          CI_low = hdi_ypred$CI_low)%>%
    add_column(x = 1:nrow(choice_set))
  
  ggplot(post_pred,aes(x,y))+
    geom_point()+
    geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
    labs(title='Group',x='Trial',y='# Option 1')+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('./RIC/output/fig/fit_prev/MHD_post_group_choice.png',
         height = 6,width = 18)
  png('./RIC/output/fig/fit_prev/MHD_pairs_group_choice.png')
  pairs(samples,pars = parameters[1:7])
  dev.off()
  png('./RIC/output/fig/fit_prev/MHD_trace_group_choice.png')
  traceplot(samples,pars = parameters[1:7])
  dev.off()
  
  post_param <- as.data.frame(summary(samples)$summary[1:7,])%>%
    rownames_to_column()
  post_param
  write_csv(post_param,'./RIC/output/results/fit_prev/MHD_param_choice.csv')
  
#}
dim(ypred)

dim(hdi_ypred)





## post inference ============



# fit mhd choice log -------------------
parameters <- c('a','c','logh_d','logh_r','logs_d','logs_r','s',
                #'sig','a_i','c_i','loghd_i','loghr_i','logsr_i','logsd_i','s_i',
                'ypred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_MHD_choice_log.stan',
                data=data,
                pars=parameters,
                iter = 12000,
                warmup = 8000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                refresh = 100,
                control = list(max_treedepth = 15,
                               adapt_delta = 0.99))
#
saveRDS(samples,
        './RIC/output/results/fit_prev/MHD_hier_choice_log.rds')

