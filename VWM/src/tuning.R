source('./VWM/src/requires.R')
rm(list=ls())
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
i <- 1
ind <- exp1_dt$ID==i

prior_ind <- 1
prior_file <- paste0('prior_',prior_ind)
pw <- paste0("./VWM/output/results/small_scale/",
             prior_file)
if(!dir.exists(pw)) dir.create(pw)

pw2 <- paste0("./VWM/output/fig/small_scale/",
              prior_file)
if(!dir.exists(pw2)) dir.create(pw2)

s_list <- c(5,10, 15, 20)
parameters <- c('ypred')
for(s in s_list){
  source("./VWM/src/small_scale.R")
}
