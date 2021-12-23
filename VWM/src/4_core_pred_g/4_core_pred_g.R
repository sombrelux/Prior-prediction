rm(list=ls())
library(rstan)
options(mc.cores = parallel::detectCores())
library(tidyverse)

# prior prediction --------------
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')
post_param <- read_csv('./VWM/output/results/fit_prev/param_im.csv')
post_param

mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2)
parameters <- 'ypred'

i=5;a_w=1;b_w=1;mu_s=5;sig_s=1
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
             sig_a = sig_post[1]*i, sig_b = sig_post[2]*i, sig_r = sig_post[3]*i,
             sig_kappa = sig_post[5]*i, sig_delta = sig_post[6]*i,
             #mu_sloc = mu_post[4], 
             mu_s = mu_s,  sig_s = sig_s, 
             a_w = a_w,b_w = b_w)

samples <- stan(
  file = './VWM/src/4_prior_pred_g/prior_IM.stan',
  data = data, 
  pars = parameters,
  iter = 1000,
  warmup = 0,
  chains = 20,
  cores = 20,
  algorithm="Fixed_param")
ypred <- rstan::extract(samples)$ypred
dim(ypred)

write.table(ypred,
            file = paste0("./VWM/output/results/prior_pred_g/IM_",
               i,"_",sig_s,"_",a_w,"_",b_w,".txt"),
            sep = ' ',
            row.names = FALSE)

# core prediction of response vs distance -----
rm(list=ls())
library(tidyverse)
library(circular)
library(bayestestR)

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')

i=1;sig_s=1;a_w=1;b_w=1
ypred <- read.table(paste0("./VWM/output/results/prior_pred_g/IM_",
                           i,"_",sig_s,"_",a_w,"_",b_w,".txt"),
                    header = TRUE)
#ytarg <- exp4_dt$m[,1] #0-2pi
ypred_rad <- ypred/180*pi #0-2pi
yntarg <- exp4_dt$m[,-1] #0-2pi

dev_nt <- array(dim = c(6300,20000,5))
for(j in 1:20000){
  y <- as.numeric(ypred_rad[j,])
  dev_nt[,j,] <- apply(yntarg,2,function(u) as.circular(wrap(y-u)))
}

## col dist ==============
Dcol <- round(exp4_dt$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist

dcol1 <- dcol2 <- dcol3 <- matrix(nrow=3,ncol = 20000)
for(k in 1:3){
  dcol1_i <- dcol2_i <- dcol3_i <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    devnt_abs_temp <- dev_nt[ind,,]
    dcol_temp <- as.matrix(Dcol[ind,2:6])
    dist_1 <- dcol_temp==dcol_uniq[1]
    dist_2 <- dcol_temp==dcol_uniq[2]
    dist_3 <- dcol_temp>dcol_uniq[2]
    error_col_1 <- apply(devnt_abs_temp,2,function(u) 1/sd.circular(u[dist_1]))
    error_col_2 <- apply(devnt_abs_temp,2,function(u) 1/sd.circular(u[dist_2]))
    error_col_3 <- apply(devnt_abs_temp,2,function(u) 1/sd.circular(u[dist_3]))
    dcol1_i <- rbind(dcol1_i,error_col_1)
    dcol2_i <- rbind(dcol2_i,error_col_2)
    dcol3_i <- rbind(dcol3_i,error_col_3)
  }
  dcol1[k,] <- colMeans(dcol1_i)
  dcol2[k,] <- colMeans(dcol2_i)
  dcol3[k,] <- colMeans(dcol3_i)
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
  hdi(.,ci = 0.9999)%>%data.frame()
dcol_ci$cond <- dcol$cond
dcol_ci$dist <- dcol$dist
dcol_ci

write_csv(dcol_ci,
          paste0("./VWM/output/results/prior_pred_g/dcol_ci_",
                 i,"_",sig_s,"_",a_w,"_",b_w,".csv"))

## loc dist =================
Dloc <- round(exp4_dt$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist

dloc1 <- dloc2 <- dloc3 <- matrix(nrow=3,
                                  ncol = 20000)
for(k in 1:3){
  dloc1_i <- dloc2_i <- dloc3_i <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    devnt_abs_temp <- dev_nt[ind,,]
    dloc_temp <- as.matrix(Dloc[ind,2:6])
    dist_1 <- dloc_temp==dloc_uniq[1]
    dist_2 <- dloc_temp==dloc_uniq[2]
    dist_3 <- dloc_temp>dloc_uniq[2]
    error_loc_1 <- apply(devnt_abs_temp,2,function(u) 1/sd.circular(u[dist_1]))
    error_loc_2 <- apply(devnt_abs_temp,2,function(u) 1/sd.circular(u[dist_2]))
    error_loc_3 <- apply(devnt_abs_temp,2,function(u) 1/sd.circular(u[dist_3]))
    dloc1_i <- rbind(dloc1_i,error_loc_1)
    dloc2_i <- rbind(dloc2_i,error_loc_2)
    dloc3_i <- rbind(dloc3_i,error_loc_3)
  }
  dloc1[k,] <- colMeans(dloc1_i)
  dloc2[k,] <- colMeans(dloc2_i)
  dloc3[k,] <- colMeans(dloc3_i)
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
  data.frame()%>%hdi(.,ci = 0.9999)%>%
  data.frame()

dloc_ci$cond <- dloc$cond
dloc_ci$dist <- dloc$dist
dloc_ci

write_csv(dloc_ci,
          paste0("./VWM/output/results/prior_pred_g/dloc_ci_",
                 i,"_",sig_s,"_",a_w,"_",b_w,".csv"))
