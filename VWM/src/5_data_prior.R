rm(list=ls())
library(tidyverse)
library(R.matlab)

# vdBerg orientations ==========
dir <- getwd()
setwd('./VWM/data/previous')
subj_files <- list.files()

exp_ind <- c(9,10)
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
  'vdBerg, 2012','RTT, 2012'
)
for(i in 1:2){
  err_temp <- err_pool[[i]]
  mae_resp_err <- err_temp%>%
    group_by(id)%>%
    summarise(mae = mean(abs(err)))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mae_resp_err)
}

mae_resp_err
mae_err_dp <- data.frame(cond=c('Both','Color','Location'),
                         DP_low = rep(min(mae_resp_err$mae)-sd(mae_resp_err$mae),3),
                         DP_high = rep(max(mae_resp_err$mae)+sd(mae_resp_err$mae),3))
write_csv(mae_err_dp,
          './VWM/output/results/data_prior/resp_err_dp.csv')

## dev_nt =============
dev_nt_pool <- 
  readRDS('./VWM/data/processed/vdBerg_dev_nt.rds')

mae_dev_nt <- data.frame()
for(i in 1:2){
  dev_nt_temp <- dev_nt_pool[[i]]
  mae_dev_nt<- dev_nt_temp%>%
    group_by(id)%>%
    summarise(mae = mean(err))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mae_dev_nt)
}

mae_dev_nt
mae_devnt_dp <- data.frame(cond=c('Both','Color','Location'),
                           DP_low = rep(min(mae_dev_nt$mae)-sd(mae_dev_nt$mae),3),
                           DP_high = rep(max(mae_dev_nt$mae)+sd(mae_dev_nt$mae),3))
write_csv(mae_devnt_dp,
          './VWM/output/results/data_prior/dev_nt_dp.csv')

# OL's exp1 ===========
rm(list=ls())
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

exp1_dt <- read.table("./VWM/data/raw/Colorwheel9.dat")
setsize_ind <- exp1_dt[,5] == 6

color <- exp1_dt[setsize_ind, c(6,8,10,12,14,16,18,20)]
location <- exp1_dt[setsize_ind, c(7,9,11,13,15,17,19,21)]
Response <- exp1_dt[setsize_ind, 22]

location_rad <- 2*pi*location/13 #0~2pi
location_dist <- location_rad[,2:6] - location_rad[,1] #-2pi~2pi
Dist <- abs(wrap(location_dist)) #0~pi
Dist <- data.frame(round(Dist,3))
colnames(Dist) <- paste0('loc',1:5)
Dist <- data.frame(id = exp1_dt[,1],
                   Dist)

color_rad <- pi*color/180-pi
resp_rad <- pi*Response/180-pi
resp_err <- wrap(resp_rad - color_rad[,1])

dev_nt <- wrap(resp_rad - color_rad[,2:6])
colnames(dev_nt) <- paste0('dev_nt',1:5)
dev_nt_abs <- data.frame(id = exp1_dt[,1],
                         abs(dev_nt))

saveRDS(Dist,
        './VWM/data/processed/OL1_Dist.rds')
saveRDS(dev_nt_abs,
        './VWM/data/processed/OL1_dev_nt.rds')

## mae of devnt vs dist ================
dev_nt_abs <- 
  readRDS('./VWM/data/processed/OL1_dev_nt.rds')
Dist <- 
  readRDS('./VWM/data/processed/OL1_Dist.rds')

devnt <- dev_nt_abs[,-1]
### loc ==================
dloc_uniq <- sort(unique(Dist$loc1))
dloc_uniq # 6 unique dist
dloc <- Dist[,-1]

error_1 <- error_2 <- error_3 <- devnt
error_1[dloc!=dloc_uniq[1]] <- NA
error_2[dloc!=dloc_uniq[2]] <- NA
error_3[dloc<dloc_uniq[3]] <- NA

err_dist_1 <- data.frame(id = Dist$id,error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae1=mean(dev_nt))

err_dist_2 <- data.frame(id = Dist$id,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae2=mean(dev_nt))

err_dist_3 <- data.frame(id = Dist$id,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae3=mean(dev_nt))

err_dloc <- merge(err_dist_1,err_dist_2)%>%
  merge(.,err_dist_3)%>%
  mutate(diff1=mae1-mae3,diff2=mae2-mae3)%>%
  dplyr::select(diff1:diff2)%>%
  pivot_longer(everything(),names_to = 'dist',values_to = 'obs')%>%
  mutate(dist = dplyr::recode(dist,"diff1" = "Dloc=1/13",
                              "diff2" = "Dloc=2/13"))
err_dloc

mae_dloc_exp1 <- err_dloc%>%
  dplyr::group_by(dist)%>%
  dplyr::summarise(DP_low=min(obs)-3*sd(obs),
                   DP_high=max(obs)+3*sd(obs))

mae_dloc_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=2),
                          mae_dloc_exp1[rep(1:nrow(mae_dloc_exp1),3),])
mae_dloc_dp
write_csv(mae_dloc_dp,
          './VWM/output/results/data_prior/dloc_dp.csv')

### col ==================
dloc_uniq
2/9*pi
4/9*pi
6/9*pi

error_1 <- error_2 <- error_3 <- devnt
error_1[dloc>dloc_uniq[3]] <- NA
error_2[dloc!=dloc_uniq[3]] <- NA
error_3[dloc<dloc_uniq[4]] <- NA

err_dist_1 <- data.frame(id = Dist$id,error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae1=mean(dev_nt))

err_dist_2 <- data.frame(id = Dist$id,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae2=mean(dev_nt))

err_dist_3 <- data.frame(id = Dist$id,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae3=mean(dev_nt))

err_dcol <- merge(err_dist_1,err_dist_2)%>%
  merge(.,err_dist_3)%>%
  mutate(diff1=mae1-mae3,diff2=mae2-mae3)%>%
  dplyr::select(diff1:diff2)%>%
  pivot_longer(everything(),names_to = 'dist',values_to = 'obs')%>%
  mutate(dist = dplyr::recode(dist,"diff1" = "Dcol=1/9",
                              "diff2" = "Dcol=2/9"))
err_dcol

mae_dcol_exp1 <- err_dcol%>%
  dplyr::group_by(dist)%>%
  dplyr::summarise(DP_low=min(obs)-3*sd(obs),
                   DP_high=max(obs)+3*sd(obs))

mae_dcol_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=2),
                          mae_dcol_exp1[rep(1:nrow(mae_dcol_exp1),3),])
write_csv(mae_dcol_dp,
          './VWM/output/results/data_prior/dcol_dp.csv')
