source('./VWM/src/requires.R')
rm(list=ls())
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
i <- 1
ind <- exp1_dt$ID==i
s_list <- c(2, 10, 20, 25)
parameters <- c('ypred')

# Tuning prior ----------------

for(prior_ind in c(7:9)){
  #prior_ind <- 6
  prior_file <- paste0('prior_',prior_ind)
  pw <- paste0("./VWM/output/results/tuning_priors/",
              prior_file)
  if(!dir.exists(pw)) dir.create(pw)
  for(s in s_list ){
    data <- list(nTrial=sum(ind), 
                 M=exp1_dt$M,N=exp1_dt$N,
                 Setsize=exp1_dt$Setsize[ind],
                 X=exp1_dt$X,
                 D=exp1_dt$D[ind,],m=exp1_dt$m[ind,],
                 s=s)
    samples <- stan(
      file=paste0('./VWM/src/tuning_priors/',
                  prior_file,'_g.stan'),
      data=data,pars=parameters,iter = 250,warmup = 0,
      seed = 123, algorithm="Fixed_param")
    saveRDS(samples,
            paste0(pw,"/s=",s,".rds"))
    rm(list=c('data','samples','s'))
    Sys.sleep(10)
  }
}
