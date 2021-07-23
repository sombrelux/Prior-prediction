source('./VWM/src/requires.R')
rm(list=ls())
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
i <- 1
ind <- exp1_dt$ID==i

# Tuning prior ----------------
prior_ind <- 7
prior_file <- paste0('prior_',prior_ind)
pw <- paste0("./VWM/output/results/tuning_priors/",
             prior_file)
if(!dir.exists(pw)) dir.create(pw)

s_list <- c(5, 10, 15, 20)
parameters <- c('ypred')
#for(s in s_list){
s=5
  data <- list(nTrial=sum(ind), 
               M=exp1_dt$M,N=exp1_dt$N,
               Setsize=exp1_dt$Setsize[ind],
               X=exp1_dt$X,
               D=exp1_dt$D[ind,],m=exp1_dt$m[ind,],
               s=s)
  samples <- stan(
    file=paste0('./VWM/src/tuning_priors/',
                prior_file,'_g.stan'),
    data=data,pars=parameters,iter = 500,warmup = 0,
    seed = 123, algorithm="Fixed_param")
  saveRDS(samples,
          paste0(pw,"/s=",s,".rds"))
  rm(list=c('data','samples','s'))
 # Sys.sleep(20)
#}
