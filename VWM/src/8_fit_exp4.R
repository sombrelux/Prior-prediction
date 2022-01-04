rm(list = ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 5)

# Fit individual ---------
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')
parameters <- c('a','b','r','sloc','scol','w',
                'kappa','delta','xpred')

data <- list(nPart=exp4_dt$nPart,
             ID = exp4_dt$ID,
             nTrial=length(exp4_dt$ID),
             N=exp4_dt$N,
             Condition = exp4_dt$Condition,
             M = exp4_dt$Setsize,
             m=exp4_dt$m,#orientations
             Dcol=exp4_dt$Dcol,#dist of col
             Dloc=exp4_dt$Dloc,#dist of loc
             E = exp4_dt$E,
             x=exp4_dt$x)
  
fit_im <- stan(file='./VWM/src/fit_im_exp4.stan',
                 data=data,
                 pars=parameters,
                 iter=2000,
                 refresh = 10,
                 warmup=1000,
                 chains=4, 
                 cores=4,
                 seed = 123)
saveRDS(fit_im,
          './VWM/output/results/fit_prev/exp4_im.rds')

png('./VWM/output/fig/fit_prev/pairs_im4.png',width = 520, height = 520)
pairs(fit_im,pars = parameters[1:8])
dev.off()
  
  png('./VWM/output/fig/fit_prev/trace_im4.png')
  traceplot(fit_im,pars = parameters[1:8])
  dev.off()
  
  post_param <- as.data.frame(summary(fit_im)$summary[1:8,])%>%
    rownames_to_column()
  post_param
  write_csv(post_param,
            './VWM/output/results/fit_prev/param_im4.csv')

# post inference ------
ypred <- extract(fit_im)$xpred
dim(ypred)

ypred_rad <- ypred/180*pi
ytarg <- exp4_dt$m[,1]
yntarg <- exp4_dt$m[,-1] #0-2pi

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
## mae of response error ========
abs_err <- apply(ypred_rad,1, function(u) abs(wrap(u-ytarg)))
dim(abs_err)
mae_err <- matrix(nrow=3,ncol = 4000)
for(k in 1:3){
  mae_err_j <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    abs_err_temp <- abs_err[ind,]
    mae_err_j <- rbind(mae_err_j,colMeans(abs_err_temp))
  }
  mae_err[k,] <- colMeans(mae_err_j)
}
mae_err <- data.frame(cond = c("Both","Color","Location"), mae_err)
mae_err_ci <- mae_err%>%
  dplyr::select(!c(cond))%>%t()%>%
  data.frame()%>%
  hdi(.,ci = 0.99)%>%data.frame()%>%
  add_column(cond = mae_err$cond)
mae_err_ci
write_csv(mae_err_ci,
          paste0("./VWM/output/results/prior_pred_unfit/mae_err_ci_",i,'_',a_w,".csv"))

## mae of dev_nt ==========
dev_nt <- array(dim = c(6300,4000,5))
for(j in 1:4000){
  y <- as.numeric(ypred_rad[j,])
  dev_nt[,j,] <- apply(yntarg,2,function(u) wrap(y-u))#as.circular(wrap(y-u)))
}

mae_nt <- matrix(nrow=3,ncol = 4000)
for(k in 1:3){
  mae_nt_j <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    devnt_temp <- abs(dev_nt[ind,,])
    mae_nt_j <- rbind(mae_nt_j, apply(devnt_temp,2, mean))
  }
  mae_nt[k,] <- colMeans(mae_nt_j)
}

mae_nt <- data.frame(cond = c("Both","Color","Location"), mae_nt)
mae_nt_ci <- mae_nt%>%
  dplyr::select(!c(cond))%>%t()%>%
  data.frame()%>%
  hdi(.,ci = 0.99)%>%data.frame()%>%
  add_column(cond = mae_nt$cond)
mae_nt_ci
write_csv(mae_nt_ci,
          paste0("./VWM/output/results/prior_pred_unfit/mae_nt_ci_",i,'_',a_w,".csv"))

## col dist ==============
Dcol <- round(exp4_dt$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist

dcol1 <- dcol2 <- dcol3 <- matrix(nrow=3,ncol = 4000)
for(k in 1:3){
  dcol1_j <- dcol2_j <- dcol3_j <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    devnt_abs_temp <- dev_nt[ind,,]
    dcol_temp <- as.matrix(Dcol[ind,2:6])
    dist_1 <- dcol_temp==dcol_uniq[1]
    dist_2 <- dcol_temp==dcol_uniq[2]
    dist_3 <- dcol_temp>dcol_uniq[2]
    prec_col_1 <- apply(devnt_abs_temp,2,function(u) mean(abs(u[dist_1])))#1/sd.circular(u[dist_1]))
    prec_col_2 <- apply(devnt_abs_temp,2,function(u) mean(abs(u[dist_2])))#1/sd.circular(u[dist_2]))
    prec_col_3 <- apply(devnt_abs_temp,2,function(u) mean(abs(u[dist_3])))#1/sd.circular(u[dist_3]))
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
  hdi(.,ci = 0.99)%>%data.frame()
dcol_ci$cond <- dcol$cond
dcol_ci$dist <- dcol$dist
dcol_ci

write_csv(dcol_ci,
          paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_",i,'_',a_w,".csv"))

## loc dist =================
Dloc <- round(exp4_dt$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist

dloc1 <- dloc2 <- dloc3 <- matrix(nrow=3,
                                  ncol = 4000)
for(k in 1:3){
  dloc1_i <- dloc2_i <- dloc3_i <- NULL
  for(j in 1:21){
    ind <- (exp4_dt$Condition==k)&(exp4_dt$ID==j)
    devnt_abs_temp <- dev_nt[ind,,]
    dloc_temp <- as.matrix(Dloc[ind,2:6])
    dist_1 <- dloc_temp==dloc_uniq[1]
    dist_2 <- dloc_temp==dloc_uniq[2]
    dist_3 <- dloc_temp>dloc_uniq[2]
    prec_loc_1 <- apply(devnt_abs_temp,2,function(u) mean(abs(u[dist_1])))#1/sd.circular(u[dist_1]))
    prec_loc_2 <- apply(devnt_abs_temp,2,function(u) mean(abs(u[dist_2])))#1/sd.circular(u[dist_2]))
    prec_loc_3 <- apply(devnt_abs_temp,2,function(u) mean(abs(u[dist_3])))#1/sd.circular(u[dist_3]))
    dloc1_i <- rbind(dloc1_i,prec_loc_1)
    dloc2_i <- rbind(dloc2_i,prec_loc_2)
    dloc3_i <- rbind(dloc3_i,prec_loc_3)
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
  data.frame()%>%hdi(.,ci = 0.99)%>%
  data.frame()

dloc_ci$cond <- dloc$cond
dloc_ci$dist <- dloc$dist
dloc_ci

write_csv(dloc_ci,
          paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_",
                 i,'_',a_w,".csv"))

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
  hdi(.,ci = 0.99)%>%data.frame()
diff_col_ci$cond <- dcol_diff$cond
diff_col_ci$dist <- dcol_diff$dist
diff_col_ci
write_csv(diff_col_ci,
          paste0("./VWM/output/results/prior_pred_unfit/diff_col_ci_",i,'_',a_w,".csv"))

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
  hdi(.,ci = 0.99)%>%data.frame()
diff_loc_ci$cond <- dloc_diff$cond
diff_loc_ci$dist <- dloc_diff$dist
diff_loc_ci
write_csv(diff_loc_ci,
          paste0("./VWM/output/results/prior_pred_unfit/diff_loc_ci_",i,'_',a_w,".csv"))

