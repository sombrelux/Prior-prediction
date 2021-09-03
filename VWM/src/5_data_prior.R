rm(list=ls())
library(tidyverse)
library(R.matlab)

# previous datasets -----------
## vdBerg orientations ==========
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
<<<<<<< HEAD
                                 err=subj_dt[[1]][setsize_ind]))
=======
                       err=subj_dt[[1]][setsize_ind]))
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
    err_nt_i <- sapply(subj_dt[[2]],
                       function(u) mean(abs(u[[1]])), 
                       simplify = T)
    err_nt_temp <- rbind(err_nt_temp,
                         data.frame(id=i,
<<<<<<< HEAD
                                    err=err_nt_i[setsize_ind]))
=======
                          err=err_nt_i[setsize_ind]))
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
  }
  
  err_pool[[j]] <- err_temp
  err_nt_pool[[j]] <- err_nt_temp
}

setwd(dir)
saveRDS(err_pool,
        './VWM/data/processed/vdBerg_resp_err.rds')

saveRDS(err_nt_pool,
        './VWM/data/processed/vdBerg_dev_nt.rds')

## OL's exp1 ===========
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

# data prior -------------
rm(list=ls())
pw <- "./VWM/output/results/data_prior"
<<<<<<< HEAD
prior_file = 'prior_0'
=======

>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
## mean abs error ===========
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
saveRDS(mae_resp_err,paste0(pw,"/mae_err_vdBerg.rds"))

### plot =========
mae_err_ci <- read_csv(
  paste0("./VWM/output/results/prior_prediction/",
         prior_file,"/mae_err_ci.csv"))

mae_err_dp <- data.frame(cond=c('Both','Color','Location'),
<<<<<<< HEAD
                         DP_low = rep(min(mae_resp_err$mae)-sd(mae_resp_err$mae),3),
                         DP_high = rep(min(max(mae_resp_err$mae)+sd(mae_resp_err$mae),pi/2),3))

=======
                         DP_low = rep(min(mae_esp_err$mae)-sd(mae_resp_err$mae),3),
                         DP_high = rep(max(mae_resp_err$mae)+sd(mae_resp_err$mae),3))
						 
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
mae_err <- merge(mae_err_ci,mae_err_dp,
                 by='cond')

ggplot(mae_err,aes(x=cond))+
<<<<<<< HEAD
  geom_errorbar(aes(ymin=lower,
                    ymax=upper,
                    col='Core prediction'),
                size=1.2,
                alpha=1,
                width = 0.2)+
=======
geom_errorbar(aes(ymin=lower,
                  ymax=upper,
                  col='Core prediction'),
              size=1.2,
              alpha=1,
              width = 0.2)+
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
  geom_errorbar(aes(ymin=DP_low,
                    ymax=DP_high,
                    col='Data prior'),
                size=1.2,
                alpha=0.8,
                width = 0.2,
                key_glyph =draw_key_smooth)+
  labs(x='Condition',y='MAE of response errors')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

##  mae of dev_nt ===========
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
saveRDS(mae_dev_nt,paste0(pw,"/mae_devnt_vdBerg.rds"))

### plot ================
mae_devnt_ci <- 
  read_csv(
    paste0("./VWM/output/results/prior_prediction/",
           prior_file,"/mae_devnt_ci.csv"))
<<<<<<< HEAD

mae_devnt_dp <- data.frame(cond=c('Both','Color','Location'),
                           DP_low = rep(min(mae_dev_nt$mae)-sd(mae_dev_nt$mae),3),
                           DP_high = rep(min(max(mae_dev_nt$mae)+sd(mae_dev_nt$mae),pi/2),3))

mae_devnt <- merge(mae_devnt_ci,mae_devnt_dp,
                   by='cond')
=======
		   
mae_devnt_dp <- data.frame(cond=c('Both','Color','Location'),
                         DP_low = rep(min(mae_dev_nt$mae)-sd(mae_dev_nt$mae),3),
                         DP_high = rep(max(mae_dev_nt$mae)+sd(mae_dev_nt$mae),3))

mae_devnt <- merge(mae_devnt_ci,mae_devnt_dp,
                 by='cond')
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e

ggplot(mae_devnt,aes(x=cond))+
  geom_errorbar(aes(ymin=lower,
                    ymax=upper,
                    col='Core prediction'),
                size=1.2,
                alpha=1,
                width = 0.2)+
  geom_errorbar(aes(ymin=DP_low,
                    ymax=DP_high,
                    col='Data prior'),
                size=1.2,
                alpha=0.8,
                width = 0.2)+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())
## mae of devnt vs dist ================
dev_nt_abs <- 
  readRDS('./VWM/data/processed/OL1_dev_nt.rds')
Dist <- 
  readRDS('./VWM/data/processed/OL1_Dist.rds')

dloc_uniq <- sort(unique(Dist$loc1))
dloc_uniq # 6 unique dist

devnt <- dev_nt_abs[,-1]
dloc <- Dist[,-1]

### loc ==================
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
  dplyr::summarise(mae=mean(dev_nt))

err_dist_2 <- data.frame(id = Dist$id,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dist_3 <- data.frame(id = Dist$id,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dloc <- data.frame(
  rbind(err_dist_1$mae, err_dist_2$mae,
        err_dist_3$mae),
  dist=c('Dloc=1/13','Dloc=2/13','Dloc>2/13'))%>%
  pivot_longer(!dist,names_to = 'id',
               values_to = 'mae')

err_dloc
saveRDS(err_dloc,
        paste0(pw,"/mae_dloc_OL1.rds"))

#### plot =============
<<<<<<< HEAD
mae_dloc_exp1 <- err_dloc%>%
  dplyr::group_by(dist)%>%
  dplyr::summarise(DP_low=min(mae)-3*sd(mae),
                   DP_high=min(max(mae)+3*sd(mae),pi/2))

mae_dloc_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=3),
                          #dist=rep(c('Dloc=1/13','Dloc=2/13','Dloc>2/13'),3),
                          mae_dloc_exp1[rep(1:nrow(mae_dloc_exp1),3),])
                          #DP_low=rep(min(mae_resp_err$mae)-sd(mae_resp_err$mae),9),
                          #DP_high=rep(pi/2,9))
=======
mae_dloc_exp1 <- mae_dloc%>%
  dplyr::group_by(dist)%>%
  dplyr::summarise(DP_low=min(mae)-3*sd(mae),
                   DP_high=max(mae)+3*sd(mae))
mae_dloc_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=3),
                          mae_dloc_exp1[rep(1:nrow(mae_dloc_exp1),3),])
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
mae_dloc_ci <- 
  read_csv(paste0("./VWM/output/results/prior_prediction/",
                  prior_file,"/mae_dloc_ci.csv"))
mae_dloc <- merge(mae_dloc_ci,mae_dloc_dp,
<<<<<<< HEAD
                  by=c('cond','dist'))

=======
                   by=c('cond','dist'))
				   
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
ggplot(mae_dloc,aes(x=dist))+
  geom_errorbar(aes(ymin=lower,
                    ymax=upper,
                    col='Core prediction'),
                size=1.2,
                alpha=1,
                width = 0.2)+
  geom_errorbar(aes(ymin=DP_low,
                    ymax=DP_high,
                    col='Data prior'),
                size=1.2,
                alpha=0.7,
                width = 0.2)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())
<<<<<<< HEAD

=======
		
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
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
  dplyr::summarise(mae=mean(dev_nt))

err_dist_2 <- data.frame(id = Dist$id,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dist_3 <- data.frame(id = Dist$id,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(id)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dcol <- data.frame(
  rbind(err_dist_1$mae,err_dist_2$mae,
        err_dist_3$mae),
  dist=c('Dcol=1/9','Dcol=2/9','Dcol>2/9'))%>%
<<<<<<< HEAD
  pivot_longer(!dist,names_to = 'id',
=======
  pivot_longer(!diff,names_to = 'id',
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
               values_to = 'mae')

err_dcol
saveRDS(err_dcol,paste0(pw,"/mae_dcol_OL1.rds"))

#### plot ===========
<<<<<<< HEAD
mae_dcol_exp1 <- err_dcol%>%
  dplyr::group_by(dist)%>%
  dplyr::summarise(DP_low=min(mae)-3*sd(mae),
                   DP_high=min(max(mae)+3*sd(mae),pi/2))

=======
mae_dcol_exp1 <- mae_dcol%>%
  dplyr::group_by(dist)%>%
  dplyr::summarise(DP_low=min(mae)-3*sd(mae),
                   DP_high=max(mae)+3*sd(mae))
mae_dcol_dp_th <- c(DP_low=min(mae_esp_err$mae)-sd(mae_resp_err$mae),DP_high=pi/2)
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e
mae_dcol_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=3),
                          mae_dcol_exp1[rep(1:nrow(mae_dcol_exp1),3),])
mae_dcol_ci <- 
  read_csv(paste0("./VWM/output/results/prior_prediction/",
                  prior_file,"/mae_dcol_ci.csv"))
mae_dcol <- merge(mae_dcol_ci,mae_dcol_dp,
<<<<<<< HEAD
                  by=c('cond','dist'))
=======
                   by=c('cond','dist'))
>>>>>>> 2c8e75ecb82346aec079ab462ff8c6974e54769e

ggplot(mae_dcol,aes(x=dist))+
  geom_errorbar(aes(ymin=lower,
                    ymax=upper,
                    col='Core prediction'),
                size=1.2,
                width = 0.2)+
  geom_errorbar(aes(ymin=DP_low,
                    ymax=DP_high,
                    col='Data prior'),
                size=1.2,
                alpha=0.7,
                width = 0.2)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())
