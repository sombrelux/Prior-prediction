rm(list=ls())
library(tidyverse)
library(HDInterval)
library(rstan)
options(mc.cores = parallel::detectCores())

pw <- "./VWM/output/results/prior_prediction/"
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')

# subj --------------
<<<<<<< HEAD:VWM/src/3_core_pred.R
prior_file <- 'prior_5'
=======
prior_file <- 'prior_2'
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e:VWM/src/4_core_pred.R
dir.create(paste0(pw,prior_file))

dir.create(paste0(pw,prior_file,'/subj'))

parameters <- c('ypred')
for(i in 16:exp4_dt$nPart){
  ind <- exp4_dt$ID==i
  data <- list(nTrial=sum(ind),
               N=exp4_dt$N,
               Condition = exp4_dt$Condition[ind],
               M = exp4_dt$Setsize,
               m=exp4_dt$m[ind,],#orientations
               Dcol=exp4_dt$Dcol[ind,],#dist of col
               Dloc=exp4_dt$Dloc[ind,],#dist of loc
               X=exp4_dt$X #360 candidate resp
  )
  samples <- stan(
    file = paste0('./VWM/src/',prior_file,'.stan'),
    data = data, pars = parameters,
    iter = 2000,
    warmup = 0,
    seed = 123,
    chains = 4,
    cores = 4,
    algorithm="Fixed_param")
  saveRDS(samples,
          paste0(pw,prior_file,"/subj/subj",i,".rds"))
  rm(list=c('samples','data','ind'))
}

## pool --------------
rm(list=ls())
<<<<<<< HEAD:VWM/src/3_core_pred.R
prior_file <- 'prior_5'
=======
prior_file <- 'prior_2'
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e:VWM/src/4_core_pred.R
pw <- "./VWM/output/results/prior_prediction/"
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')

ytrue <- ypred_pool <- m <- cond <- 
  Dcol <- Dloc <-  NULL

for(i in 1:exp4_dt$nPart){
  samples <- readRDS(paste0(pw,prior_file,
                            "/subj/subj",i,".rds"))
  ypred <- t(rstan::extract(samples)$ypred)
  ind <- exp4_dt$ID==i
  data <- list(Condition = exp4_dt$Condition[ind],
               m=exp4_dt$m[ind,],#orientations
               Dcol=exp4_dt$Dcol[ind,],#dist of col
               Dloc=exp4_dt$Dloc[ind,],#dist of loc
               ytrue = exp4_dt$response[ind]
  )
  m <- rbind(m,data$m)
  cond <- c(cond,data$Condition)
  Dloc <- rbind(Dloc,data$Dloc)
  Dcol <- rbind(Dcol,data$Dcol)
  ypred_pool <- rbind(ypred_pool, ypred/180*pi)
  ytrue <- c(ytrue, data$ytrue)
  
}
dim(ypred_pool)

prior_pred <- list(
  Orient = m,
  Condition = cond,
  Dloc = Dloc,
  Dcol = Dcol,
  ypred = ypred_pool,
  ytrue = ytrue
)

saveRDS(prior_pred,paste0(pw,prior_file,
                          "/prior_pred.rds"))

# core prediction -------------
rm(list=ls())

pw <- "./VWM/output/results/prior_prediction/"
<<<<<<< HEAD:VWM/src/3_core_pred.R
prior_file <- 'prior_5'
=======
prior_file <- 'prior_2'
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e:VWM/src/4_core_pred.R
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
<<<<<<< HEAD:VWM/src/3_core_pred.R
                   function(u) wrap(u-ytarg)))
=======
                       function(u) wrap(u-ytarg)))
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e:VWM/src/4_core_pred.R
dim(error_prior)

mae_err_prior <- abs(error_prior)%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise_at(vars(X1:X8000),~mean(.))%>%
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
Dcol <- round(prior_pred$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist

Dloc <- round(prior_pred$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist

error_dcol <- error_dloc <- NULL
for(i in 1:3){
  cond_ind <- prior_pred$Condition==i
  devnt_abs_temp <- dev_nt_abs[cond_ind,,]
  
  dcol_temp <- as.matrix(Dcol[cond_ind,2:6])
  error_col_1 <- apply(devnt_abs_temp,2,
                       function(u) mean(u[dcol_temp==dcol_uniq[1]]))
  error_col_2 <- apply(devnt_abs_temp,2,
                       function(u) mean(u[dcol_temp==dcol_uniq[2]]))
  error_col_3 <- apply(devnt_abs_temp,2,
                       function(u) mean(u[dcol_temp>dcol_uniq[2]]))
  error_dcol_temp <- data.frame(
    rbind(error_col_1,
          error_col_2,error_col_3),
    dist = c('Dcol=1/9','Dcol=2/9','Dcol>2/9'),
    cond=i)
  error_dcol <- rbind(error_dcol,error_dcol_temp)
  
  dloc_temp <- as.matrix(Dloc[cond_ind,2:6])
  error_loc_1 <- apply(devnt_abs_temp,2,
                       function(u) mean(u[dloc_temp==dloc_uniq[1]]))
  error_loc_2 <- apply(devnt_abs_temp,2,
                       function(u) mean(u[dloc_temp==dloc_uniq[2]]))
  error_loc_3 <- apply(devnt_abs_temp,2,
                       function(u) mean(u[dloc_temp>dloc_uniq[2]]))
  error_dloc_temp <- data.frame(
    rbind(error_loc_1,
          error_loc_2,error_loc_3),
    dist = c('Dloc=1/13','Dloc=2/13','Dloc>2/13'),
    cond=i)
  error_dloc <- rbind(error_dloc,error_dloc_temp)
}
error_dcol <- error_dcol%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
error_dloc <- error_dloc%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))

saveRDS(error_dcol,
        paste0(pw,prior_file,"/mae_dcol.rds"))
saveRDS(error_dloc,
        paste0(pw,prior_file,"/mae_dloc.rds"))

error_dcol_ci <-   error_dcol%>%
  dplyr::select(!c(cond,dist))%>%
  t()%>%
  hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()
error_dcol_ci$cond <- error_dcol$cond
error_dcol_ci$dist <-error_dcol$dist
error_dcol_ci
write_csv(error_dcol_ci,
          paste0(pw,prior_file,"/mae_dcol_ci.csv"))

error_dloc_ci <- error_dloc%>%
  dplyr::select(!c(cond,dist))%>%
  t()%>%hdi(.,credMass = 0.999)%>%t()%>%
  data.frame()
error_dloc_ci$cond <- error_dloc$cond
error_dloc_ci$dist <- error_dloc$dist
error_dloc_ci
write_csv(error_dloc_ci,
          paste0(pw,prior_file,"/mae_dloc_ci.csv"))
