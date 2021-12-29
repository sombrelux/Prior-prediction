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
Dist <- data.frame(ID,Dist)

color_rad <- pi*color/180
resp_rad <- pi*Response/180
dev_nt <- wrap(resp_rad - color_rad[,2:6])
colnames(dev_nt) <- paste0('dev_nt',1:5)
dev_nt <- data.frame(id = exp1_dt[setsize_ind,1],dev_nt)

ave_prec <- function(data, ind){
  sample_bt <- data[ind,] 
  return(mean(sample_bt$prec))
}

# precision -------------
## loc ==================
dloc_uniq <- sort(unique(Dist$loc1))
dloc_uniq # 6 unique dist
dloc <- Dist[,-1]

error_1 <- error_2 <- error_3 <- dev_nt[,-1]
error_1[dloc!=dloc_uniq[1]] <- NA
error_2[dloc!=dloc_uniq[2]] <- NA
error_3[dloc<dloc_uniq[3]] <- NA

err_dist_1 <- data.frame(id = Dist$ID,error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=1/sd.circular(dev_nt))
set.seed(1)
bootstrap_prec1 <- boot(err_dist_1,ave_prec,R=10000)
dloc_ci_1 <- c(mean(bootstrap_prec1$t)-5*sd(bootstrap_prec1$t),
               mean(bootstrap_prec1$t)+5*sd(bootstrap_prec1$t))

err_dist_2 <- data.frame(id = Dist$ID,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(prec=1/sd.circular(dev_nt))
set.seed(1)
bootstrap_prec2 <- boot(err_dist_2,ave_prec,R=10000)
dloc_ci_2 <- c(mean(bootstrap_prec2$t)-5*sd(bootstrap_prec2$t),
               mean(bootstrap_prec2$t)+5*sd(bootstrap_prec2$t))

err_dist_3 <- data.frame(id = Dist$ID,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(prec=1/sd.circular(dev_nt))
set.seed(1)
bootstrap_prec3 <- boot(err_dist_3,ave_prec,R=10000)
dloc_ci_3 <- c(mean(bootstrap_prec3$t)-5*sd(bootstrap_prec3$t),
               mean(bootstrap_prec3$t)+5*sd(bootstrap_prec3$t))


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

error_1 <- error_2 <- error_3 <- dev_nt[,-1]
error_1[dloc>dloc_uniq[2]] <- NA
error_2[dloc!=dloc_uniq[3]] <- NA
error_3[dloc<dloc_uniq[4]] <- NA

err_dist_1 <- data.frame(id = Dist$ID,error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=1/sd.circular(dev_nt))
set.seed(1)
bootstrap_prec1 <- boot(err_dist_1,ave_prec,R=10000)
dcol_ci_1 <- c(mean(bootstrap_prec1$t)-10*sd(bootstrap_prec1$t),
               mean(bootstrap_prec1$t)+10*sd(bootstrap_prec1$t))

err_dist_2 <- data.frame(id = Dist$ID,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=1/sd.circular(dev_nt))
set.seed(1)
bootstrap_prec2 <- boot(err_dist_2,ave_prec,R=10000)
dcol_ci_2 <- c(mean(bootstrap_prec2$t)-10*sd(bootstrap_prec2$t),
               mean(bootstrap_prec2$t)+10*sd(bootstrap_prec2$t))

err_dist_3 <- data.frame(id = Dist$ID,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(prec=1/sd.circular(dev_nt))
set.seed(1)
bootstrap_prec3 <- boot(err_dist_3,ave_prec,R=10000)
dcol_ci_3 <- c(mean(bootstrap_prec3$t)-10*sd(bootstrap_prec3$t),
               mean(bootstrap_prec3$t)+10*sd(bootstrap_prec3$t))

dcol_exp1 <- rbind(dcol_ci_1,dcol_ci_2,dcol_ci_3)
dcol_exp1 <- dcol_exp1%>%as.data.frame()%>%
  rename(CI_low=V1,CI_high=V2)%>%
  add_column(dist = c('=1/9','=2/9','>2/9'))

dcol_exp1
write_csv(dcol_exp1,
          './VWM/output/results/data_prior/dcol_exp1.csv')

# differences -------------
## loc ==================