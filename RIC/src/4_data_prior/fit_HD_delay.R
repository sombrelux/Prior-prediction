rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
#Sys.setenv(STAN_NUM_THREADS = 4)

delay_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)%>%
  filter(p1==1,p2==1)
  
# individ ----------------
Set_list <- unique(delay_set$Exp)
Set_list
i <- Set_list[2]
delay_temp <- delay_set#%>%filter(Exp==i)
dim(delay_temp)
data <- list(
  nTrial = nrow(delay_temp),
  x1 = delay_temp$x1, x2 = delay_temp$x2,
  t1 = delay_temp$t1, t2 = delay_temp$t2,
  o1 = 1/delay_temp$p1-1,
  o2 = 1/delay_temp$p2-1,
  N = delay_temp$N,
  y = delay_temp$y)
parameters <- c('a','logh','s',
                'ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_HD_delay_ind.stan',
                data=data,
                pars=parameters,
                iter = 4000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
pairs(samples,pars = parameters[1:3])
traceplot(samples,pars = parameters[1:3])
saveRDS(samples,
        paste0('./RIC/output/results/data_prior/HD_',i,'.rds'))
