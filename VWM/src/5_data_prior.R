rm(list=ls())
library(tidyverse)
library(boot)
library(circular)

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

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

ave_prec <- function(data, ind){
  sample_bt <- data[ind,] 
  return(mean(sample_bt$prec))
}
ave_diff <- function(data, ind){
  sample_bt <- data[ind] 
  return(mean(sample_bt))
}
# response error ---------
abs_err <- abs(wrap(resp_rad - color_rad[,1]))
mae_err <- data.frame(id = ID, abs_err = abs_err)%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(abs_err))
set.seed(1)
bt_abs_err <- boot(mae_err$mae,ave_diff,R=10000)
bt_abs_err_ci <- c(mean(bt_abs_err$t)-5*sd(bt_abs_err$t),
               mean(bt_abs_err$t)+5*sd(bt_abs_err$t))
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
bt_dev_nt <- boot(mae_nt$mae,ave_diff,R=10000)
bt_dev_nt_ci <- c(mean(bt_dev_nt$t)-5*sd(bt_dev_nt$t),
                   mean(bt_dev_nt$t)+5*sd(bt_dev_nt$t))
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
  dplyr::summarise(prec=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
set.seed(1)
bt_prec_loc1 <- boot(prec_loc1,ave_prec,R=10000)
dloc_ci_1 <- c(mean(bt_prec_loc1$t)-5*sd(bt_prec_loc1$t),
               mean(bt_prec_loc1$t)+5*sd(bt_prec_loc1$t))

prec_loc2 <- data.frame(id = ID, dloc_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(prec=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
set.seed(1)
bt_prec_loc2 <- boot(prec_loc2,ave_prec,R=10000)
dloc_ci_2 <- c(mean(bt_prec_loc2$t)-5*sd(bt_prec_loc2$t),
               mean(bt_prec_loc2$t)+5*sd(bt_prec_loc2$t))

prec_loc3 <- data.frame(id = ID, dloc_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(prec=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
set.seed(1)
bt_prec_loc3 <- boot(prec_loc3,ave_prec,R=10000)
dloc_ci_3 <- c(mean(bt_prec_loc3$t)-5*sd(bt_prec_loc3$t),
               mean(bt_prec_loc3$t)+5*sd(bt_prec_loc3$t))


dloc_exp1 <- rbind(dloc_ci_1,dloc_ci_2,dloc_ci_3)
dloc_exp1 <- dloc_exp1%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('=1/13','=2/13','>2/13'))

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
  dplyr::summarise(prec=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
set.seed(1)
bt_prec_col1 <- boot(prec_col1,ave_prec,R=10000)
dcol_ci_1 <- c(mean(bt_prec_col1$t)-5*sd(bt_prec_col1$t),
               mean(bt_prec_col1$t)+5*sd(bt_prec_col1$t))

prec_col2 <- data.frame(id = ID, dcol_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
set.seed(1)
bt_prec_col2 <- boot(prec_col2,ave_prec,R=10000)
dcol_ci_2 <- c(mean(bt_prec_col2$t)-5*sd(bt_prec_col2$t),
               mean(bt_prec_col2$t)+5*sd(bt_prec_col2$t))

prec_col3 <- data.frame(id = ID, dcol_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=mean(abs(dev_nt)))#1/sd.circular(dev_nt))
set.seed(1)
bt_prec_col3 <- boot(prec_col3,ave_prec,R=10000)
dcol_ci_3 <- c(mean(bt_prec_col3$t)-5*sd(bt_prec_col3$t),
               mean(bt_prec_col3$t)+5*sd(bt_prec_col3$t))

dcol_exp1 <- rbind(dcol_ci_1,dcol_ci_2,dcol_ci_3)
dcol_exp1 <- dcol_exp1%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('=1/9','=2/9','>2/9'))

dcol_exp1
write_csv(dcol_exp1,
          './VWM/output/results/data_prior/dcol_exp1.csv')

# differences -------------
## loc ==================
diff_loc1 <- prec_loc1$prec-prec_loc2$prec
set.seed(1)
bt_diff_loc1 <- boot(diff_loc1,ave_diff,R=10000)
diff_loc_ci1 <- c(mean(bt_diff_loc1$t)-5*sd(bt_diff_loc1$t),
                  mean(bt_diff_loc1$t)+5*sd(bt_diff_loc1$t))

diff_loc2 <- prec_loc2$prec-prec_loc3$prec
set.seed(1)
bt_diff_loc2 <- boot(diff_loc2,ave_diff,R=10000)
diff_loc_ci2 <- c(mean(bt_diff_loc2$t)-5*sd(bt_diff_loc2$t),
                  mean(bt_diff_loc2$t)+5*sd(bt_diff_loc2$t))
				  
diff_loc_ci <- rbind(diff_loc_ci1,diff_loc_ci2)
diff_loc_ci <- diff_loc_ci%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('1 - 2','2 - 3+'))
diff_loc_ci
write_csv(diff_loc_ci,
          './VWM/output/results/data_prior/diff_loc_ci.csv')

## col ==================
diff_col1 <- prec_col1$prec-prec_col2$prec
set.seed(1)
bt_diff_col1 <- boot(diff_col1,ave_diff,R=10000)
diff_col_ci1 <- c(mean(bt_diff_col1$t)-5*sd(bt_diff_col1$t),
                  mean(bt_diff_col1$t)+5*sd(bt_diff_col1$t))

diff_col2 <- prec_col2$prec-prec_col3$prec
set.seed(1)
bt_diff_col2 <- boot(diff_col2,ave_diff,R=10000)
diff_col_ci2 <- c(mean(bt_diff_col2$t)-5*sd(bt_diff_col2$t),
                  mean(bt_diff_col2$t)+5*sd(bt_diff_col2$t))
				  
diff_col_ci <- rbind(diff_col_ci1,diff_col_ci2)
diff_col_ci <- diff_col_ci%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('1 - 2','2 - 3+'))
diff_col_ci
write_csv(diff_col_ci,
          './VWM/output/results/data_prior/diff_col_ci.csv')
