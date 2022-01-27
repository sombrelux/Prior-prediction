rm(list=ls())
library(tidyverse)
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')
dir.create('./VWM/output/results/testing')

# mae of resp err ---------------
resp_error_abs <- abs(wrap(exp4_dt$response-exp4_dt$m[,1]))
mae_err_obs <-  data.frame(err=resp_error_abs,
                           cond=exp4_dt$Condition,
                           id = exp4_dt$ID)%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(err_i=mean(err))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(obs=mean(err_i))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
mae_err_obs
write_csv(mae_err_obs,
          './VWM/output/results/testing/mae_err_exp4.csv')
# dev_nt -----------
devnt_obs <- wrap(exp4_dt$response-exp4_dt$m[,-1])
colnames(devnt_obs) <- paste0('dev_nt',1:5)

mae_nt <- data.frame(id = exp4_dt$ID,
                     cond = exp4_dt$Condition,
                     devnt_obs)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(mae=mean(abs(dev_nt)))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(obs=mean(mae))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))
mae_nt
write_csv(mae_nt,
          './VWM/output/results/testing/mae_nt_exp4.csv')

# dev_nt vs dist -----------------
## loc ==================
Dloc <- round(exp4_dt$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist
dloc <- Dloc[,-1]

dloc1 <- dloc2 <- dloc3 <- devnt_obs
dloc1[dloc!=dloc_uniq[1]] <- NA
dloc2[dloc!=dloc_uniq[2]] <- NA
dloc3[dloc<dloc_uniq[3]] <- NA

prec_loc1 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dloc1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec1=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
ave_prec_loc1 <- prec_loc1%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec1))%>%
  add_column(dist = '=0.48')
  
prec_loc2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dloc2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec2=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
ave_prec_loc2 <- prec_loc2%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec2))%>%
  add_column(dist = '=0.97')

prec_loc3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dloc3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec3=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
ave_prec_loc3 <- prec_loc3%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec3))%>%
  add_column(dist = '>0.97')

dloc_exp4 <- rbind(ave_prec_loc1,ave_prec_loc2,ave_prec_loc3)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
dloc_exp4
write_csv(dloc_exp4,
          './VWM/output/results/testing/dloc_exp4.csv')
## col ==================
Dcol <- round(exp4_dt$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist
dcol <- Dcol[,-1]

dcol1 <- dcol2 <- dcol3 <- devnt_obs
dcol1[dcol!=dcol_uniq[1]] <- NA
dcol2[dcol!=dcol_uniq[2]] <- NA
dcol3[dcol<dcol_uniq[3]] <- NA

prec_col1 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dcol1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec1=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
ave_prec_col1 <- prec_col1%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec1))%>%
  add_column(dist = '=0.7')


prec_col2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dcol2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec2=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
ave_prec_col2 <- prec_col2%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec2))%>%
  add_column(dist = '=1.4')

prec_col3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dcol3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec3=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
ave_prec_col3 <- prec_col3%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec3))%>%
  add_column(dist = '>1.4')

dcol_exp4 <- rbind(ave_prec_col1,ave_prec_col2,ave_prec_col3)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
dcol_exp4
write_csv(dcol_exp4,
          './VWM/output/results/testing/dcol_exp4.csv')
