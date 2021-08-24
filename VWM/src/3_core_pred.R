rm(list=ls())
library(tidyverse)
library(bayestestR)
library(rstan)
options(mc.cores = parallel::detectCores())

pw <- "./VWM/output/results/prior_prediction/"

# subj --------------
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')
parameters <- c('ypred')
for(i in 1:exp4_dt$nPart){
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
    file = './VWM/src/inform_prior_2.stan',
    data = data, pars = parameters,
    iter = 2000,
    warmup = 0,
    seed = 123,
    chains = 4,
    cores = 4,
    algorithm="Fixed_param")
  saveRDS(samples,
          paste0(pw,"/subj/subj",i,".rds"))
  rm(list=c('samples','data','ind'))
  Sys.sleep(1)
}

## pool --------------
ytrue <- ypred_pool <- m <- cond <- 
  Dcol <- Dloc <-  NULL

for(i in 1:exp4_dt$nPart){
  samples <- readRDS(paste0(pw,"/subj/subj",i,".rds"))
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

saveRDS(prior_pred,paste0(pw,"prior_pred.rds"))

# core prediction -------------
prior_pred <- readRDS(paste0(pw,"prior_pred.rds"))
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
ypred_rad <- prior_pred$ypred

## mae of response errors ============
ytarg <- prior_pred$Orient[,1]
error_prior_abs <- 
  data.frame(cond = prior_pred$Condition,
             abs(apply(ypred_rad,2,
                       function(u) wrap(u-ytarg))))
dim(error_prior_abs)

mae_err_prior <- error_prior_abs%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise_at(vars(X1:X8000),~mean(.))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))%>%
  column_to_rownames(var='cond')
dim(mae_err_prior)
saveRDS(mae_err_prior,paste0(pw,"/pred_mae_err.rds"))

mae_err_ci <- mae_err_prior%>%t()%>%
  data.frame()%>%hdi(.,ci=0.99)
colnames(mae_err_ci)[1] <- 'cond'
mae_err_ci
write_csv(mae_err_ci,paste0(pw,"/pred_mae_err_ci.csv"))

mae_err_prior <- mae_err_prior%>%
  rownames_to_column(var = "cond")%>%
  pivot_longer(!cond,names_to = 'iter',
               values_to = 'mae')

ggplot(mae_err_prior, aes(x=cond))+
  geom_point(aes(y=mae),alpha=0.5,
             size=2.5,
             col="#56B4E9")+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                data=mae_err_ci,
                size=1.2,
                col='darkred',
                width = 0.2)+
  labs(x='Condition',y='MAE of response errors')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))

ggsave("./VWM/output/fig/pred_resp_err_mae.png",
       height=4, width = 4)

## mae of dev_nt =========
dev_nt_abs <- array(dim = c(6300,8000,5))
yntarg <- prior_pred$Orient[,-1]
for(i in 1:5){
  dev_nt_abs[,,i] <-  apply(ypred_rad,2,
                function(u) abs(wrap(u-yntarg[,i])))
}
dim(dev_nt_abs)

dev_nt_temp <- dev_nt_abs[,1:500,]
mean_devnt_abs <- apply(dev_nt_temp,c(1,2),mean)

mae_devnt_prior <- data.frame(cond=prior_pred$Condition,
                              dev_nt_abs)
dim(mae_devnt_prior)

mae_devnt_prior <- error_prior_abs%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise_at(vars(X1:X8000),~mean(.))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))%>%
  column_to_rownames(var='cond')
dim(mae_err_prior)
saveRDS(mae_err_prior,paste0(pw,"/pred_mae_err.rds"))