rm(list=ls())
library(rstan)
options(mc.cores = parallel::detectCores())
library(tidyverse)
library(data.table)

# prior prediction --------------
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')
post_param <- read_csv('./VWM/output/results/fit_prev/param_im.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- read.csv('./VWM/src/4_core_pred_unfit/sig_IM.csv',header = T)
parameters <- 'ypred'
i <- 5;Ub_s <- 20

for(k in 1:5){
  data <- list(nPart=exp4_dt$nPart,
               ID = exp4_dt$ID,
               nTrial=length(exp4_dt$ID),
               N=exp4_dt$N,
               Condition = exp4_dt$Condition,
               M = exp4_dt$Setsize,
               m=exp4_dt$m,#orientations
               Dcol=exp4_dt$Dcol,#dist of col
               Dloc=exp4_dt$Dloc,#dist of loc
               X=exp4_dt$X, #360 candidate resp
               mu_a = mu_post[1], mu_b = mu_post[2], mu_r = mu_post[3],
               mu_kappa = mu_post[5], mu_delta = mu_post[6],
               sig_a = sig_post[1,i], sig_b = sig_post[2,i], sig_r = sig_post[3,i], 
               sig_kappa = sig_post[4,i], sig_delta = sig_post[5,i],
               Ub_s = Ub_s)
  
  samples <- stan(
    file = './VWM/src/4_core_pred_unfit/prior_IM_unif.stan',
    data = data, 
    pars = parameters,
    iter = 1000,
    warmup = 0,
    chains = 20,
    cores = 20,
    algorithm="Fixed_param")
  ypred <- rstan::extract(samples)$ypred
  
  fwrite(ypred, paste0("./VWM/output/results/prior_pred_unfit/IM_",
                       i,'_',Ub_s,'_',k,'.csv'))
}

# core prediction of response vs distance -----
rm(list=ls())
library(tidyverse)
library(data.table)
library(HDInterval)

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')

i=5;Ub_s=20
ypred1 <- fread(paste0("./VWM/output/results/prior_pred_unfit/IM_",
                           i,'_',Ub_s,'_1','.csv'))/180*pi
ypred2 <- fread(paste0("./VWM/output/results/prior_pred_unfit/IM_",
                       i,'_',Ub_s,"_2.csv"))/180*pi
ypred3 <- fread(paste0("./VWM/output/results/prior_pred_unfit/IM_",
                       i,'_',Ub_s,"_3.csv"))/180*pi
ypred4 <- fread(paste0("./VWM/output/results/prior_pred_unfit/IM_",
                       i,'_',Ub_s,"_4.csv"))/180*pi
ypred5 <- fread(paste0("./VWM/output/results/prior_pred_unfit/IM_",
                       i,'_',Ub_s,"_5.csv"))/180*pi

ypred_rad <- rbind(ypred1,ypred2,ypred3,ypred4,ypred5)
rm(list=c('ypred1','ypred2','ypred3','ypred4','ypred5'))
dim(ypred_rad)

ytarg <- exp4_dt$m[,1] #0-2pi
yntarg <- exp4_dt$m[,-1] #0-2pi

## mae of response error ========
abs_err <- apply(ypred_rad,1, function(u) abs(wrap(u-ytarg)))
dim(abs_err)
mae_err <- matrix(nrow=3,ncol = 100000)
for(k in 1:3){
  mae_err_j <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    abs_err_temp <- abs_err[ind,]
    mae_err_j <- rbind(mae_err_j,colMeans(abs_err_temp))
  }
  mae_err[k,] <- colMeans(mae_err_j)
}
mae_err_ci <- mae_err%>%t()%>%
  data.frame()%>%
  hdi(.,credMass = 0.9999)%>%t()%>%
  data.frame()%>%
  add_column(cond = c('Both','Color','Location'))%>%
  rename(CI_low=lower,CI_high=upper)
mae_err_ci
write_csv(mae_err_ci,
          paste0("./VWM/output/results/prior_pred_unfit/mae_err_ci_",i,'_',Ub_s,".csv"))

## mae of dev_nt ==========
dev_nt <- list()#array(dim = c(6300,80000,5))
for(j in 1:100000){
  y <- as.numeric(ypred_rad[j,])
  dev_nt[[j]] <- apply(yntarg,2,function(u) wrap(y-u))
}

mae_nt <- matrix(nrow=3,ncol = 100000)
for(k in 1:3){
  mae_nt_k <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    devnt_temp <- lapply(dev_nt,function(u) abs(u[ind,]))
    mae_nt_temp <- sapply(devnt_temp, rowMeans)
    mae_nt_k <- rbind(mae_nt_k, mae_nt_temp)
  }
  mae_nt[k,] <- colMeans(mae_nt_k)
}

mae_nt_ci <- mae_nt%>%t()%>%
  data.frame()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%
  add_column(cond = c('Both','Color','Location'))%>%
  rename(CI_low=lower,CI_high=upper)
mae_nt_ci
write_csv(mae_nt_ci,
          paste0("./VWM/output/results/prior_pred_unfit/mae_nt_ci_",i,'_',Ub_s,"0.csv"))

## col dist ==============
Dcol <- round(exp4_dt$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist

dcol1 <- dcol2 <- dcol3 <- matrix(nrow=3,ncol = 100000)
for(k in 1:3){
  dcol1_j <- dcol2_j <- dcol3_j <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    dcol_temp <- as.matrix(Dcol[ind,2:6])
    dist_1 <- dcol_temp==dcol_uniq[1]
    dist_2 <- dcol_temp==dcol_uniq[2]
    dist_3 <- dcol_temp>dcol_uniq[2]
    
    devnt_temp <- lapply(dev_nt,function(u) u[ind,])
    prec_col_1 <- sapply(devnt_temp,function(u) mean(abs(u[dist_1])))
    prec_col_2 <- sapply(devnt_temp,function(u) mean(abs(u[dist_2])))
    prec_col_3 <- sapply(devnt_temp,function(u) mean(abs(u[dist_3])))
    dcol1_j <- rbind(dcol1_j,prec_col_1)
    dcol2_j <- rbind(dcol2_j,prec_col_2)
    dcol3_j <- rbind(dcol3_j,prec_col_3)
  }
  dcol1[k,] <- colMeans(dcol1_j)
  dcol2[k,] <- colMeans(dcol2_j)
  dcol3[k,] <- colMeans(dcol3_j)
}

dcol1 <- data.frame(
  cond = 1:3, dist = 'Dcol=1/9',
  dcol1)
dcol2 <-   data.frame(
  cond = 1:3, dist = 'Dcol=2/9',
  dcol2)
dcol3 <-   data.frame(
  cond = 1:3, dist = 'Dcol>2/9',
  dcol3)
dcol <- rbind(dcol1,dcol2,dcol3)
dim(dcol)

dcol <- dcol%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
dcol$cond
dcol$dist
dim(dcol)

dcol_ci <- dcol%>%
  dplyr::select(!c(cond,dist))%>%t()%>%
  data.frame()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%
  rename(CI_low=lower,CI_high=upper)
  
dcol_ci$cond <- dcol$cond
dcol_ci$dist <- dcol$dist
dcol_ci

write_csv(dcol_ci,
          paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_",i,'_',Ub_s,"0.csv"))

## loc dist =================
Dloc <- round(exp4_dt$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist

dloc1 <- dloc2 <- dloc3 <- matrix(nrow=3,
                                  ncol = 100000)
for(k in 1:3){
  dloc1_j <- dloc2_j <- dloc3_j <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    dloc_temp <- as.matrix(Dloc[ind,2:6])
    dist_1 <- dloc_temp==dloc_uniq[1]
    dist_2 <- dloc_temp==dloc_uniq[2]
    dist_3 <- dloc_temp>dloc_uniq[2]
    
    devnt_temp <- lapply(dev_nt,function(u) u[ind,])
    prec_loc_1 <- sapply(devnt_temp,function(u) mean(abs(u[dist_1])))
    prec_loc_2 <- sapply(devnt_temp,function(u) mean(abs(u[dist_2])))
    prec_loc_3 <- sapply(devnt_temp,function(u) mean(abs(u[dist_3])))
    dloc1_j <- rbind(dloc1_j,prec_loc_1)
    dloc2_j <- rbind(dloc2_j,prec_loc_2)
    dloc3_j <- rbind(dloc3_j,prec_loc_3)
  }
  dloc1[k,] <- colMeans(dloc1_j)
  dloc2[k,] <- colMeans(dloc2_j)
  dloc3[k,] <- colMeans(dloc3_j)
}

dloc1 <- data.frame(
  cond = 1:3, dist = 'Dloc=1/13',
  dloc1)
dloc2 <-   data.frame(
  cond = 1:3, dist = 'Dloc=2/13',
  dloc2)
dloc3 <-   data.frame(
  cond = 1:3, dist = 'Dloc>2/13',
  dloc3)
dloc <- rbind(dloc1,dloc2,dloc3)
dim(dloc)

dloc <- dloc%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
dloc$cond
dloc$dist

dloc_ci <- dloc%>%
  dplyr::select(!c(cond,dist))%>%t()%>%
  data.frame()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%
  rename(CI_low=lower,CI_high=upper)

dloc_ci$cond <- dloc$cond
dloc_ci$dist <- dloc$dist
dloc_ci

write_csv(dloc_ci,
          paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_",
                 i,'_',Ub_s,"0.csv"))

# difference 1 vs 2, 2 vs 3+ --------------
## color diff ================
diff_col1 <- data.frame(
  cond = 1:3, dist = '1 - 2',
  dcol1[,-(1:2)]-dcol2[,-(1:2)])
diff_col2 <-   data.frame(
  cond = 1:3, dist = '2 - 3+',
  dcol2[,-(1:2)]-dcol3[,-(1:2)])
dcol_diff <- rbind(diff_col1,diff_col2)
dcol_diff <- dcol_diff%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
diff_col_ci <- dcol_diff%>%
  dplyr::select(!c(cond,dist))%>%t()%>%
  data.frame()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%
  rename(CI_low=lower,CI_high=upper)
diff_col_ci$cond <- dcol_diff$cond
diff_col_ci$dist <- dcol_diff$dist
diff_col_ci
write_csv(diff_col_ci,
          paste0("./VWM/output/results/prior_pred_unfit/diff_col_ci_",i,'_',Ub_s,"0.csv"))

## loc dist =================
diff_loc1 <- data.frame(
  cond = 1:3, dist = '1 - 2',
  dloc1[,-(1:2)]-dloc2[,-(1:2)])
diff_loc2 <-   data.frame(
  cond = 1:3, dist = '2 - 3+',
  dloc2[,-(1:2)]-dloc3[,-(1:2)])
dloc_diff <- rbind(diff_loc1,diff_loc2)
dloc_diff <- dloc_diff%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
diff_loc_ci <- dloc_diff%>%
  dplyr::select(!c(cond,dist))%>%t()%>%
  data.frame()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%
  rename(CI_low=lower,CI_high=upper)
diff_loc_ci$cond <- dloc_diff$cond
diff_loc_ci$dist <- dloc_diff$dist
diff_loc_ci
write_csv(diff_loc_ci,
          paste0("./VWM/output/results/prior_pred_unfit/diff_loc_ci_",i,'_',Ub_s,"0.csv"))

