rm(list=ls())
library(tidyverse)
library(boot)

dir.create('./VWM/output/results/data_prior')

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
boot_func <- function(data, ind){
  sample_bt <- data[ind] 
  return(mean(sample_bt))
}

# OL 1 -----------------
exp1_dt <- read.table("./VWM/data/raw/Colorwheel9.dat")
setsize_ind <- exp1_dt[,5] == 6
ID <- exp1_dt[setsize_ind,1]
color <- exp1_dt[setsize_ind, c(6,8,10,12,14,16,18,20)]
location <- exp1_dt[setsize_ind, c(7,9,11,13,15,17,19,21)]
Response <- exp1_dt[setsize_ind, 22]

location_rad <- 2*pi*location/13 #0~2pi
location_dist <- location_rad[,2:6] - location_rad[,1] #-2pi~2pi
Dist <- abs(wrap(location_dist)) #0~pi
Dist <- data.frame(round(Dist,3))
colnames(Dist) <- paste0('loc',1:5)

color_rad <- pi*color/180
resp_rad <- pi*Response/180

# response error ---------
abs_err <- abs(wrap(resp_rad - color_rad[,1]))
mae_err <- data.frame(id = ID, abs_err = abs_err)%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(abs_err))
set.seed(1)
bt_abs_err <- boot(mae_err$mae,boot_func,R=10000)
bt_abs_err_ci <- c(bt_abs_err$t0-5*sd(bt_abs_err$t),
               bt_abs_err$t0+5*sd(bt_abs_err$t))
bt_abs_err_ci
saveRDS(bt_abs_err_ci,'./VWM/output/results/data_prior/mae_err_exp1.rds')

# dev_nt -------------
dev_nt <- wrap(resp_rad - color_rad[,2:6])
colnames(dev_nt) <- paste0('dev_nt',1:5)
mae_nt <- data.frame(id = ID, dev_nt)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(abs(dev_nt)))
set.seed(1)
bt_dev_nt <- boot(mae_nt$mae,boot_func,R=10000)
bt_dev_nt_ci <- c(bt_dev_nt$t0-5*sd(bt_dev_nt$t),
                  bt_dev_nt$t0+5*sd(bt_dev_nt$t))
bt_dev_nt_ci
saveRDS(bt_dev_nt_ci,'./VWM/output/results/data_prior/mae_nt_exp1.rds')

# devnt vs dist -------------
## loc ==================
dloc_uniq <- sort(unique(Dist$loc1))
dloc_uniq # 6 unique dist

dloc_1 <- dloc_2 <- dloc_3 <- dev_nt
dloc_1[Dist!=dloc_uniq[1]] <- NA
dloc_2[Dist!=dloc_uniq[2]] <- NA
dloc_3[Dist<dloc_uniq[3]] <- NA

prec_loc1 <- data.frame(id = ID, dloc_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=mean(abs(dev_nt)))
set.seed(1)
bt_prec_loc1 <- boot(prec_loc1$prec,boot_func,R=10000)
dloc_ci_1 <- c(bt_prec_loc1$t0-5*sd(bt_prec_loc1$t),
               bt_prec_loc1$t0+5*sd(bt_prec_loc1$t))

prec_loc2 <- data.frame(id = ID, dloc_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(prec=mean(abs(dev_nt)))
set.seed(1)
bt_prec_loc2 <- boot(prec_loc2$prec,boot_func,R=10000)
dloc_ci_2 <- c(bt_prec_loc2$t0-5*sd(bt_prec_loc2$t),
               bt_prec_loc2$t0+5*sd(bt_prec_loc2$t))

prec_loc3 <- data.frame(id = ID, dloc_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(prec=mean(abs(dev_nt)))
set.seed(1)
bt_prec_loc3 <- boot(prec_loc3$prec,boot_func,R=10000)
dloc_ci_3 <- c(bt_prec_loc3$t0-5*sd(bt_prec_loc3$t),
               bt_prec_loc3$t0+5*sd(bt_prec_loc3$t))


dloc_exp1 <- rbind(dloc_ci_1,dloc_ci_2,dloc_ci_3)
dloc_exp1 <- dloc_exp1%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('=0.48','=0.97','>0.97'))

dloc_exp1
write_csv(dloc_exp1,
          './VWM/output/results/data_prior/dloc_exp1.csv')

## col ==================
#dloc_uniq: 0.483 0.967 1.450 1.933 2.417 2.900
#dcol_uniq in exp4: 0.698 1.396 2.094 2.793

dcol_1 <- dcol_2 <- dcol_3 <- dev_nt
dcol_1[Dist>dloc_uniq[2]] <- NA
dcol_2[Dist!=dloc_uniq[3]] <- NA
dcol_3[Dist<dloc_uniq[4]] <- NA

prec_col1 <- data.frame(id = ID,dcol_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=mean(abs(dev_nt)))
set.seed(1)
bt_prec_col1 <- boot(prec_col1$prec,boot_func,R=10000)
dcol_ci_1 <- c(bt_prec_col1$t0-10*sd(bt_prec_col1$t),
               bt_prec_col1$t0+10*sd(bt_prec_col1$t))

prec_col2 <- data.frame(id = ID, dcol_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=mean(abs(dev_nt)))
set.seed(1)
bt_prec_col2 <- boot(prec_col2$prec,boot_func,R=10000)
dcol_ci_2 <- c(bt_prec_col2$t0-10*sd(bt_prec_col2$t),
               bt_prec_col2$t0+10*sd(bt_prec_col2$t))

prec_col3 <- data.frame(id = ID, dcol_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=mean(abs(dev_nt)))
set.seed(1)
bt_prec_col3 <- boot(prec_col3$prec,boot_func,R=10000)
dcol_ci_3 <- c(bt_prec_col3$t0-10*sd(bt_prec_col3$t),
               bt_prec_col3$t0+10*sd(bt_prec_col3$t))

dcol_exp1 <- rbind(dcol_ci_1,dcol_ci_2,dcol_ci_3)
dcol_exp1 <- dcol_exp1%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('=0.7','=1.4','>1.4'))

dcol_exp1
write_csv(dcol_exp1,
          './VWM/output/results/data_prior/dcol_exp1.csv')
