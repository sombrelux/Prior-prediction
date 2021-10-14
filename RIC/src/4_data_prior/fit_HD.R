rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)
Set_list <- unique(choice_set$Exp)
Set_list
choice_set$Exp_ind <- rep(0,length(choice_set$Exp))
for(i in 1:length(Set_list)){
  choice_set$Exp_ind[choice_set$Exp==Set_list[i]] <- i
}

data<-list(
  nExp = length(Set_list),
  Exp = choice_set$Exp_ind,
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = choice_set$N,
  y = choice_set$y)

parameters <- c('a','logh','i','s',
                'a_i','logh_i','i_i','s_i',
                'sd_i','ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_HD.stan',
                data=data,
                pars=parameters,
                iter = 6000,
                warmup = 3000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
pairs(samples,pars = parameters[1:4])
traceplot(samples,pars = parameters[1:4])
saveRDS(samples,
        './RIC/output/results/data_prior/HD_hier.rds')
