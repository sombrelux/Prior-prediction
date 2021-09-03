rm(list=ls())
library(tidyverse)
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
pw <- './VWM/output/fig/'
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')

prior_file <- 'prior_2'
dir.create(paste0(pw,prior_file))

# mae of resp err ---------------
## dp =========
# loc has similar mae for color & orient
# color & loc same good cue for orient
mae_err_exp1 <- readRDS("./VWM/output/results/data_prior/mae_err_vdBerg.rds")
mae_err_dp <- data.frame(cond=c('Both','Color','Location'),
                         DP_low = rep(min(mae_err_exp1$mae)-sd(mae_err_exp1$mae),3),
                         DP_high = rep(max(mae_err_exp1$mae)+sd(mae_err_exp1$mae),3))
## observed ======
resp_error_abs <- abs(wrap(exp4_dt$response-exp4_dt$m[,1]))
mae_err_obs <-  data.frame(err=resp_error_abs,
                           cond=exp4_dt$Condition)%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(obs=mean(err))%>%
  dplyr::mutate(cond = dplyr::recode(cond,"1" = "Both",
                                     "2" = "Color",
                                     "3" = "Location"))

## core pred =====
mae_err_ci <- read_csv(
  paste0("./VWM/output/results/prior_prediction/",
         prior_file,"/mae_err_ci.csv"))

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
## dp =========
# loc has similar mae for color & orient
# color & loc same good cue for orient
mae_devnt_exp1 <- readRDS("./VWM/output/results/data_prior/mae_devnt_vdBerg.rds")
mae_devnt_dp <- data.frame(cond=c('Both','Color','Location'),
                         DP_low = rep(min(mae_devnt_exp1$mae)-sd(mae_devnt_exp1$mae),3),
                         DP_high = rep(max(mae_devnt_exp1$mae)+sd(mae_devnt_exp1$mae),3))
## observed ======
devnt_abs_obs <- abs(wrap(exp4_dt$response-exp4_dt$m[,-1]))
mae_devnt_obs <- 
  data.frame(err=apply(devnt_abs_obs, 1,mean),
             cond=exp4_dt$Condition)%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(obs=mean(err))%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))

## core pred =====
mae_devnt_ci <- 
  read_csv(
    paste0("./VWM/output/results/prior_prediction/",
           prior_file,"/mae_devnt_ci.csv"))

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
## dp =========
# loc has similar mae for color & orient
# color & loc same good cue for orient
mae_dloc_exp1 <- readRDS("./VWM/output/results/data_prior/mae_dloc_OL1.rds")
mae_dloc_exp1 <- mae_dloc_exp1%>%
  dplyr::group_by(diff)%>%
  dplyr::summarise(DP_low=min(mae)-3*sd(mae),
                   DP_high=max(mae)+3*sd(mae))
mae_dloc_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=2),
                          mae_dloc_exp1[rep(1:nrow(mae_dloc_exp1),3),])

mae_dcol_exp1 <- readRDS("./VWM/output/results/data_prior/mae_dcol_OL1.rds")
mae_dcol_exp1 <- mae_dcol_exp1%>%
  dplyr::group_by(diff)%>%
  dplyr::summarise(DP_low=min(mae)-5*sd(mae),
                   DP_high=max(mae)+5*sd(mae))
mae_dcol_dp <- data.frame(cond=rep(c('Both','Color','Location'),each=2),
                          mae_dcol_exp1[rep(1:nrow(mae_dcol_exp1),3),])
## observed ======
devnt_abs_obs <- abs(wrap(exp4_dt$response-exp4_dt$m[,-1]))
colnames(devnt_abs_obs) <- paste0('dev_nt',1:5)

### loc ==================
Dloc <- round(exp4_dt$Dloc,3)
dloc_uniq <- sort(unique(Dloc[,2]))
dloc_uniq # 6 unique dist
dloc <- Dloc[,-1]

error_1 <- error_2 <- error_3 <- devnt_abs_obs
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
  dplyr::group_by(cond)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dist_2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dist_3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mae=mean(dev_nt))

mae_dloc_obs <- data.frame(
  cbind(err_dist_1$mae-err_dist_3$mae,
        err_dist_2$mae-err_dist_3$mae),
  cond=c('1','2','3'))%>%
  rename('1-3'=X1,'2-3'=X2)%>%
  pivot_longer(!cond,names_to = 'diff',
               values_to = 'obs')%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
### col ==================
Dcol <- round(exp4_dt$Dcol,3)
dcol_uniq <- sort(unique(Dcol[,2]))
dcol_uniq # 4 unique dist
dcol <- Dcol[,-1]

error_1 <- error_2 <- error_3 <- devnt_abs_obs
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
  dplyr::group_by(cond)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dist_2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mae=mean(dev_nt))

err_dist_3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         error_3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(mae=mean(dev_nt))

mae_dcol_obs <- data.frame(
  cbind(err_dist_1$mae-err_dist_3$mae,
        err_dist_2$mae-err_dist_3$mae),
  cond=c('1','2','3'))%>%
  rename('1-3'=X1,'2-3'=X2)%>%
  pivot_longer(!cond,names_to = 'diff',
               values_to = 'obs')%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
## core pred =====
mae_dloc_ci <- 
  read_csv(paste0("./VWM/output/results/prior_prediction/",
                  prior_file,"/diff_mae_dloc_ci.csv"))
mae_dcol_ci <- 
  read_csv(paste0("./VWM/output/results/prior_prediction/",
                  prior_file,"/diff_mae_dcol_ci.csv"))

## plot =====
mae_dloc <- merge(mae_dloc_ci,mae_dloc_dp,
                   by=c('cond','diff'))%>%
  merge(.,mae_dloc_obs,by=c('cond','diff'))
ggplot(mae_dloc,aes(x=diff))+
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
                  by=c('cond','diff'))%>%
  merge(.,mae_dcol_obs,by=c('cond','diff'))
ggplot(mae_dcol,aes(x=diff))+
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
