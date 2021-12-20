rm(list=ls())
library(tidyverse)

# theoretical upper limit -----------------
x <- runif(100000,-pi,pi)
hist(x)
mean(abs(x)) #~1.57, pi/2

# OL's exp1 -------------

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

color_rad <- pi*color/180-pi
resp_rad <- pi*Response/180-pi
resp_err <- wrap(resp_rad-color_rad[,1])
mean(abs(resp_err))

dev_nt <- wrap(resp_rad - color_rad[,2:6])
colnames(dev_nt) <- paste0('dev_nt',1:5)
dev_nt_abs <- data.frame(id = exp1_dt[setsize_ind,1],
                         abs(dev_nt))

### loc ==================
dloc_uniq <- sort(unique(Dist$loc1))
dloc_uniq # 6 unique dist
dloc <- Dist[,-1]

error_1 <- error_2 <- error_3 <- dev_nt_abs[,-1]
error_1[dloc!=dloc_uniq[1]] <- NA
error_2[dloc!=dloc_uniq[2]] <- NA
error_3[dloc<dloc_uniq[3]] <- NA

err_dist_1 <- data.frame(id = Dist$ID,error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae1=mean(dev_nt))

err_dist_2 <- data.frame(id = Dist$ID,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(mae2=mean(dev_nt))

err_dist_3 <- data.frame(id = Dist$ID,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%  
  dplyr::summarise(mae3=mean(dev_nt))

err_dloc <- merge(err_dist_1,err_dist_2)%>%
  merge(.,err_dist_3)%>%
  dplyr::select(mae1:mae3)%>%
  pivot_longer(everything(),names_to = 'Dist',
               values_to = 'obs')%>%
  mutate(Dist = dplyr::recode(Dist,
                              "mae1" = "=1/13",
                              "mae2" = "=2/13",
                              "mae3" = ">2/13"))

err_dloc

dloc_exp1 <- err_dloc%>%
  dplyr::group_by(Dist)%>%
  dplyr::summarise(CI_low=mean(obs),
                   CI_high=mean(obs))
dloc_exp1

write_csv(dloc_exp1,
          './VWM/output/results/data_prior/dloc_exp1.csv')

### col ==================
#dloc_uniq: 0.483 0.967 1.450 1.933 2.417 2.900
#dcol_uniq in exp4: 0.698 1.396 2.094 2.793

error_1 <- error_2 <- error_3 <- dev_nt_abs[,-1]
error_1[dloc>dloc_uniq[2]] <- NA
error_2[dloc!=dloc_uniq[3]] <- NA
error_3[dloc<dloc_uniq[4]] <- NA

err_dist_1 <- data.frame(id = Dist$ID,error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae1=mean(dev_nt))

err_dist_2 <- data.frame(id = Dist$ID,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae2=mean(dev_nt))

err_dist_3 <- data.frame(id = Dist$ID,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae3=mean(dev_nt))

err_dcol <- merge(err_dist_1,err_dist_2)%>%
  merge(.,err_dist_3)%>%
  dplyr::select(mae1:mae3)%>%
  pivot_longer(everything(),names_to = 'Dist',values_to = 'obs')%>%
  mutate(Dist = dplyr::recode(Dist,
                              "mae1" = "=1/9",
                              "mae2" = "=2/9",
                              "mae3" = ">2/9"))
err_dcol

dcol_exp1 <- err_dcol%>%
  dplyr::group_by(Dist)%>%
  dplyr::summarise(CI_low=mean(obs),
                    CI_high=mean(obs))
dcol_exp1

write_csv(dcol_exp1,
          './VWM/output/results/data_prior/dcol_exp1.csv')

# vdBerg orientations ----------------
library(R.matlab)
dir <- getwd()
setwd('./VWM/data/previous')
subj_files <- list.files()

exp_ind <- c(2,3,7,8,9,10)

err_pool <- err_nt_pool <- list()
for(j in 1:length(exp_ind)){
  err_temp <- err_nt_temp <- NULL
  file_ind <- grep(paste0('E',exp_ind[j],'_'),
                   subj_files)
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    setsize_ind <- subj_dt[[3]]==6
    err_temp <- rbind(err_temp,
                      data.frame(id=i,
                                 err=subj_dt[[1]][setsize_ind]))
    err_nt_i <- sapply(subj_dt[[2]],
                       function(u) mean(abs(u[[1]])), 
                       simplify = T)
    err_nt_temp <- rbind(err_nt_temp,
                         data.frame(id=i,
                                    err=err_nt_i[setsize_ind]))
  }
  
  err_pool[[j]] <- err_temp
  err_nt_pool[[j]] <- err_nt_temp
}

setwd(dir)
saveRDS(err_pool,
        './VWM/data/processed/vdBerg_resp_err.rds')

saveRDS(err_nt_pool,
        './VWM/data/processed/vdBerg_dev_nt.rds')

## resp err ===========
err_pool <-
  readRDS('./VWM/data/processed/vdBerg_resp_err.rds')
mae_resp_err <- data.frame()
exp_label <- c(
  '2','3','6','7','vdBerg, 2012','RTT, 2012'
)
for(i in 1:6){
  err_temp <- err_pool[[i]]
  mae_resp_err <- err_temp%>%
    group_by(id)%>%
    summarise(mae = mean(abs(err)))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mae_resp_err)
}

mae_resp_err
write_csv(mae_resp_err,
          './VWM/output/results/data_prior/resp_err_dp.csv')

## dev_nt =============
dev_nt_pool <- 
  readRDS('./VWM/data/processed/vdBerg_dev_nt.rds')

mae_dev_nt <- data.frame()
for(i in 1:6){
  dev_nt_temp <- dev_nt_pool[[i]]
  mae_dev_nt<- dev_nt_temp%>%
    group_by(id)%>%
    summarise(mae = mean(err))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mae_dev_nt)
}

mae_dev_nt
write_csv(mae_dev_nt,
          './VWM/output/results/data_prior/dev_nt_dp.csv')
