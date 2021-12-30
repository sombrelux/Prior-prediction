rm(list=ls())
library(tidyverse)
library(circular)
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')

# mae of resp err ---------------
## observed ======
resp_error_abs <- abs(wrap(exp4_dt$response-exp4_dt$m[,1]))
mae_err_obs <-  data.frame(err=resp_error_abs,
                           cond=exp4_dt$Condition)%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(obs=mean(err))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))

## testing =====
mae_err_ci <- read_csv(
  paste0("./VWM/output/results/prior_prediction/",
         prior_file,"/mae_err_ci.csv"))
mae_err_dp <- read_csv(
  "./VWM/output/results/data_prior/resp_err_dp.csv")


## plot =====
mae_err <- merge(mae_err_ci,mae_err_dp,
                            by='cond')%>%
  merge(.,mae_err_obs,by='cond')
mae_err
ggplot(mae_err,aes(x=cond))+
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
                width = 0.2,
                key_glyph =draw_key_smooth)+
  geom_point(aes(y=obs,
                 size='Observation'),
             key_glyph =draw_key_point)+
  labs(x='Condition',y='MAE of response errors')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

ggsave(paste0(pw,prior_file,"/mae_resp_err.png"),
       height=4, width = 5)

# mae of dev_nt -------------
library(circular)
## observed ======
devnt_abs_obs <- wrap(exp4_dt$response-exp4_dt$m[,-1])
mae_devnt_obs <- 
  data.frame(err=apply(devnt_abs_obs, 1,function(u) 1/sd.circular(u)),
             cond=exp4_dt$Condition)%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(obs=mean(err))%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
## test =====
mae_devnt_ci <- read_csv(
  paste0("./VWM/output/results/prior_prediction/",
         prior_file,"/mae_devnt_ci.csv"))
mae_devnt_dp <- read_csv(
  './VWM/output/results/data_prior/dev_nt_dp.csv')


## plot =====
mae_devnt <- merge(mae_devnt_ci,mae_devnt_dp,
                   by='cond')%>%
  merge(.,mae_devnt_obs,by='cond')

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
  geom_point(aes(y=obs,
                 size='Observation'))+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

ggsave(paste0(pw,prior_file,
              "/mae_dev_nt.png"),
       height=4, width = 5)

# dev_nt vs dist -----------------
## observed ======
devnt_obs <- wrap(exp4_dt$response-exp4_dt$m[,-1])
colnames(devnt_obs) <- paste0('dev_nt',1:5)

### loc ==================
Dloc <- round(exp4_dt$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist
dloc <- Dloc[,-1]

error_1 <- error_2 <- error_3 <- devnt_obs
error_1[dloc!=dloc_uniq[1]] <- NA
error_2[dloc!=dloc_uniq[2]] <- NA
error_3[dloc<dloc_uniq[3]] <- NA

err_dist_1 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec1=1/sd.circular(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mp1=mean(prec1))
  
err_dist_2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec2=1/sd.circular(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mp2=mean(prec2))

err_dist_3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec3=1/sd.circular(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mp3=mean(prec3))

err_dloc <- merge(err_dist_1,err_dist_2)%>%
  merge(.,err_dist_3)%>%
  mutate(diff1=mp1-mp2,diff2=mp2-mp3)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
err_dloc

### col ==================
Dcol <- round(exp4_dt$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist
dcol <- Dcol[,-1]

error_1 <- error_2 <- error_3 <- devnt_obs
error_1[dcol!=dcol_uniq[1]] <- NA
error_2[dcol!=dcol_uniq[2]] <- NA
error_3[dcol<dcol_uniq[3]] <- NA

err_dist_1 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_1)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec1=1/sd.circular(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mp1=mean(prec1))


err_dist_2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec2=1/sd.circular(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mp2=mean(prec2))

err_dist_3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec3=1/sd.circular(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mp3=mean(prec3))

err_dcol <- merge(err_dist_1,err_dist_2)%>%
  merge(.,err_dist_3)%>%
  mutate(diff1=mp1-mp2,diff2=mp2-mp3)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
err_dcol

## test =====
i = 4
dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_", i,".csv"))%>%
  mutate(dist = str_replace(dist, "Dcol", ""))
dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')

dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_", i,".csv"))%>%
  mutate(dist = str_replace(dist, "Dloc", ""))
dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')

## plot =====
ggplot(dloc_ci,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Core prediction'),
                size=1.2,
                alpha=1,
                width = 0.2)+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Data prior'),
                size=1.2,
                alpha=0.7,
                width = 0.2,
                data = dloc_dp)+
  geom_point(aes(y=obs,
                 size='Observation'))+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

ggsave(paste0(pw,prior_file,"/mae_dloc.png"),
       height=4, width = 8)

mae_dcol <- merge(mae_dcol_ci,mae_dcol_dp,
                  by=c('cond','dist'))%>%
  merge(.,err_dcol,
                  by=c('cond','dist'))
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
  geom_point(aes(y=obs,
                 size='Observation'))+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

ggsave(paste0(pw,prior_file,"/mae_dcol.png"),
       height=4, width = 8)
