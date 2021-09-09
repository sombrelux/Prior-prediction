rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)
pw <- './RIC/output/results/fit_indiff/'

# fit indifference points -----------
# money below 15000, delay below 6 years
indiff_set <- 
  read_csv("./RIC/data/previous/indiff.csv")
dim(indiff_set)
indiff_set <- indiff_set%>%
				mutate(xs = -1,ts = sign(Delay),ps = sign(1-Probability),
				xd = Indifferences - Amounts,
				td = Delay, pd = 1-Probability,
				xr = 2*(Indifferences - Amounts)/(Indifferences + Amounts),
				tr = ifelse(Delay==0,0,2),
				pr = 2*(1-Probability)/(1+Probability),
				y = round(N*0.5))

indiff_set$Exp_ind <- rep(0,length(indiff_set$Exp))
Set_list <- unique(indiff_set$Exp)
for(i in 1:length(Set_list)){
  ind <- indiff_set$Exp==Set_list[i]
  indiff_set$Exp_ind[ind] <- i
}
parameters <- c('beta_xo','beta_to','beta_po',
                'beta_xa','beta_xr',
                'beta_pa','beta_pr',
                'beta_ta','beta_tr')

data<-list(
	nTrial=nrow(indiff_set),nExp=length(Set_list),
	N = indiff_set$N, Exp = indiff_set$Exp_ind,
	xs = indiff_set$xs,ts = indiff_set$ts,ps = indiff_set$ps,
	xd = indiff_set$xd,td = indiff_set$td,pd = indiff_set$pd,
	xr = indiff_set$xr,tr = indiff_set$tr,pr = indiff_set$pr,
	y = indiff_set$y)
samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_indiff.stan',
                  data=data,
                  pars=parameters,
                  iter=4000,
                  warmup = 2000,
                  chains=4, 
                  thin=4,
                  cores=4,
                  seed = 123)
saveRDS(samples,
          paste0(pw,"RITCH_indiff_",Set,".rds"))
post_stasts <- summary(samples)
write.csv(post_stasts$summary,
          paste0(pw,'RITCH_indiff_',Set,'_stats.csv'))
  
# fit available data sets ------------
prev_set <- read_csv("./RIC/data/processed/prev_df.csv")
