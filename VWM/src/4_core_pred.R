rm(list=ls())
library(tidyverse)
library(HDInterval)
library(rstan)
options(mc.cores = parallel::detectCores())

pw <- "./VWM/output/results/prior_prediction/"
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')

# prior prediction --------------
prior_file <- 'prior_broad_hier'
dir.create(paste0(pw,prior_file))
parameters <- c('ypred')
data <- list(nPart=exp4_dt$nPart,
             ID = exp4_dt$ID,
             nTrial=length(exp4_dt$ID),
             N=exp4_dt$N,
             Condition = exp4_dt$Condition,
             M = exp4_dt$Setsize,
             m=exp4_dt$m,#orientations
             Dcol=exp4_dt$Dcol,#dist of col
             Dloc=exp4_dt$Dloc,#dist of loc
             X=exp4_dt$X #360 candidate resp
)
samples <- stan(
  file = paste0('./VWM/src/',prior_file,'.stan'),
  data = data, pars = parameters,
  iter = 2000,
  warmup = 0,
  chains = 4,
  cores = 4,
  algorithm="Fixed_param")

ypred <- t(rstan::extract(samples)$ypred)
dim(ypred)

prior_pred <- list(
  ID = exp4_dt$ID,
  Orient = exp4_dt$m,
  Condition = exp4_dt$Condition,
  Dloc = exp4_dt$Dloc,
  Dcol = exp4_dt$Dcol,
  ypred = ypred/180*pi,
  ytrue = exp4_dt$response
)
saveRDS(prior_pred,paste0(pw,prior_file,
                          "/prior_pred.rds"))

# core prediction -------------
rm(list=ls())

pw <- "./VWM/output/results/prior_prediction/"
prior_file <- 'prior_broad_hier'
prior_pred <- readRDS(paste0(pw,prior_file,
                             "/prior_pred.rds"))
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
ypred_rad <- prior_pred$ypred

## mae of response errors ============
ytarg <- prior_pred$Orient[,1]
error_prior <- 
  data.frame(cond = prior_pred$Condition,
             apply(ypred_rad,2,
                   function(u) wrap(u-ytarg)))
dim(error_prior)

mae_err_prior <- abs(error_prior)%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise_at(vars(X1:X4000),~mean(.))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))%>%
  column_to_rownames(var='cond')
dim(mae_err_prior)
saveRDS(mae_err_prior,paste0(pw,prior_file,
                             "/mae_err.rds"))

mae_err_ci <- mae_err_prior%>%t()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%rownames_to_column(var = 'cond')
mae_err_ci
write_csv(mae_err_ci,paste0(pw,prior_file,
                            "/mae_err_ci.csv"))

## mae of dev_nt =========
dev_nt_abs <- array(dim = c(6300,8000,5))
yntarg <- prior_pred$Orient[,-1]
for(i in 1:5){
  dev_nt_abs[,,i] <-  apply(ypred_rad,2,
                            function(u) abs(wrap(u-yntarg[,i])))
}
dim(dev_nt_abs)
saveRDS(dev_nt_abs,
        paste0(pw,prior_file,"/dev_nt_abs.rds"))

mae_devnt <- matrix(nrow=3,ncol=8000)
for(i in 1:3){
  dev_nt_abs_temp <- dev_nt_abs[prior_pred$Condition==i,,]
  mae_devnt[i,] <- apply(dev_nt_abs_temp, 2, mean)
}
dim(mae_devnt)

mae_devnt <- data.frame(cond=c("Both","Color","Location"),
                        mae_devnt)%>%
  column_to_rownames(var='cond')
saveRDS(mae_devnt,paste0(pw,prior_file,"/mae_devnt.rds"))

mae_devnt_ci <- mae_devnt%>%t()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()%>%rownames_to_column(var = 'cond')
mae_devnt_ci
write_csv(mae_devnt_ci,paste0(pw,prior_file,"/mae_devnt_ci.csv"))

## mae of dev_nt vs distance ===============
dev_nt_abs <- readRDS(paste0(pw,prior_file,"/dev_nt_abs.rds"))

### col dist ==============
Dcol <- round(prior_pred$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist

dcol1 <- dcol2 <- matrix(nrow=3,ncol = 8000)
for(i in 1:3){
  diff1_i <- diff2_i <- NULL
  for(j in 1:21){
    ind <- (prior_pred$Condition==i)&(prior_pred$ID==j)
    devnt_abs_temp <- dev_nt_abs[ind,,]
    dcol_temp <- as.matrix(Dcol[ind,2:6])
    dist_1 <- dcol_temp==dcol_uniq[1]
    dist_2 <- dcol_temp==dcol_uniq[2]
    dist_3 <- dcol_temp>dcol_uniq[2]
    error_col_1 <- apply(devnt_abs_temp,2,function(u) mean(u[dist_1]))
    error_col_2 <- apply(devnt_abs_temp,2,function(u) mean(u[dist_2]))
    error_col_3 <- apply(devnt_abs_temp,2,function(u) mean(u[dist_3]))
    diff1_i <- rbind(diff1_i,error_col_1-error_col_3)
    diff2_i <- rbind(diff2_i,error_col_2-error_col_3)
  }
  dcol1[i,] <- colMeans(diff1_i)
  dcol2[i,] <- colMeans(diff2_i)
}

dcol1 <- data.frame(
  cond = 1:3, dist = 'Dcol=1/9',
  dcol1)
dcol2 <-   data.frame(
  cond = 1:3, dist = 'Dcol=2/9',
  dcol2)
diff_dcol <- rbind(dcol1,dcol2)
dim(diff_dcol)

diff_dcol <- diff_dcol%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
diff_dcol$cond
diff_dcol$dist
saveRDS(diff_dcol,
        paste0(pw,prior_file,"/diff_mae_dcol.rds"))

diff_dcol_ci <-   diff_dcol%>%
  dplyr::select(!c(cond,dist))%>%
  t()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()
diff_dcol_ci$cond <- diff_dcol$cond
diff_dcol_ci$dist <- diff_dcol$dist
diff_dcol_ci
write_csv(diff_dcol_ci,
          paste0(pw,prior_file,"/diff_mae_dcol_ci.csv"))

### loc dist =================
Dloc <- round(prior_pred$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist

dloc1 <- dloc2 <- matrix(nrow=3,ncol = 8000)
for(i in 1:3){
  diff1_i <- diff2_i <- NULL
  for(j in 1:21){
    ind <- (prior_pred$Condition==i)&(prior_pred$ID==j)
    devnt_abs_temp <- dev_nt_abs[ind,,]
    dloc_temp <- as.matrix(Dloc[ind,2:6])
    dist_1 <- dloc_temp==dloc_uniq[1]
    dist_2 <- dloc_temp==dloc_uniq[2]
    dist_3 <- dloc_temp>dloc_uniq[2]
    error_col_1 <- apply(devnt_abs_temp,2,function(u) mean(u[dist_1]))
    error_col_2 <- apply(devnt_abs_temp,2,function(u) mean(u[dist_2]))
    error_col_3 <- apply(devnt_abs_temp,2,function(u) mean(u[dist_3]))
    diff1_i <- rbind(diff1_i,error_col_1-error_col_3)
    diff2_i <- rbind(diff2_i,error_col_2-error_col_3)
  }
  dloc1[i,] <- colMeans(diff1_i)
  dloc2[i,] <- colMeans(diff2_i)
}

dloc1 <- data.frame(
  cond = 1:3, dist = 'Dloc=1/13',
  dloc1)
dloc2 <-   data.frame(
  cond = 1:3, dist = 'Dloc=2/13',
  dloc2)
diff_dloc <- rbind(dloc1,dloc2)
dim(diff_dloc)

diff_dloc <- diff_dloc%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
diff_dloc$cond
diff_dloc$dist
saveRDS(diff_dloc,
        paste0(pw,prior_file,"/diff_mae_dloc.rds"))

diff_dloc_ci <- diff_dloc%>%
  dplyr::select(!c(cond,dist))%>%
  t()%>%hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()
diff_dloc_ci$cond <- diff_dloc$cond
diff_dloc_ci$dist <- diff_dloc$dist
diff_dloc_ci
write_csv(diff_dloc_ci,
          paste0(pw,prior_file,"/diff_mae_dloc_ci.csv"))
