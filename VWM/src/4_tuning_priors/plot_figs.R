source('./VWM/src/requires.R')
rm(list=ls())
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
i <- 1
ind <- exp1_dt$ID==i

# tuning priors -------------
prior_ind <- 6
prior_file <- paste0('prior_',prior_ind)
pw <- paste0("./VWM/output/results/tuning_priors/",
             prior_file)
pw2 <- paste0("./VWM/output/fig/tuning_priors/",
              prior_file)
if(!dir.exists(pw2)) dir.create(pw2)

s <- 25
data <- list(nTrial=sum(ind), 
               M=exp1_dt$M,N=exp1_dt$N,
               Setsize=exp1_dt$Setsize[ind],
               X=exp1_dt$X,
               D=exp1_dt$D[ind,],m=exp1_dt$m[ind,],
               s=s)
samples <- readRDS(paste0(pw,"/s=",s,".rds"))
source('./VWM/src/tuning_priors/func_fig.R')

