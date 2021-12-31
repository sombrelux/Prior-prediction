rm(list=ls())
library(tidyverse)
library(circular)
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
exp4_dt <- readRDS('./VWM/data/processed/IM_exp4.rds')

# precision vs dist -----------------
devnt_obs <- wrap(exp4_dt$response-exp4_dt$m[,-1])
colnames(devnt_obs) <- paste0('dev_nt',1:5)

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
  dplyr::summarise(prec1=1/sd.circular(dev_nt))
ave_prec_loc1 <- prec_loc1%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec1))%>%
  add_column(dist = '=1/13')
  
prec_loc2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dloc2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec2=1/sd.circular(dev_nt))
ave_prec_loc2 <- prec_loc2%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec2))%>%
  add_column(dist = '=2/13')

prec_loc3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dloc3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec3=1/sd.circular(dev_nt))
ave_prec_loc3 <- prec_loc3%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec3))%>%
  add_column(dist = '>2/13')

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
  dplyr::summarise(prec1=1/sd.circular(dev_nt))
ave_prec_col1 <- prec_col1%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec1))%>%
  add_column(dist = '=1/9')


prec_col2 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dcol2)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec2=1/sd.circular(dev_nt))
ave_prec_col2 <- prec_col2%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec2))%>%
  add_column(dist = '=2/9')

prec_col3 <- data.frame(id = exp4_dt$ID,
                         cond = exp4_dt$Condition,
                         dcol3)%>%
  pivot_longer(dev_nt1:dev_nt5,
               names_to = 'item',
               values_to = 'dev_nt')%>%
  filter(!is.na(dev_nt))%>%
  dplyr::group_by(cond,id)%>%
  dplyr::summarise(prec3=1/sd.circular(dev_nt))
ave_prec_col3 <- prec_col3%>%
  dplyr::group_by(cond)%>%
  dplyr::summarise(prec=mean(prec3))%>%
  add_column(dist = '>2/9')

dcol_exp4 <- rbind(ave_prec_col1,ave_prec_col2,ave_prec_col3)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
dcol_exp4
write_csv(dcol_exp4,
          './VWM/output/results/testing/dcol_exp4.csv')

# difference between precisions ------------
## loc ==================
diff_loc1 <- merge(prec_loc1,prec_loc2)%>%
  mutate(diff1=prec1-prec2)%>%
  dplyr::group_by(cond)%>%
  summarise(diff=mean(diff1))%>%
  dplyr::select(cond,diff)%>%
  add_column(dist='1 - 2')

diff_loc2 <- merge(prec_loc2,prec_loc3)%>%
  mutate(diff2=prec2-prec3)%>%
  dplyr::group_by(cond)%>%
  summarise(diff=mean(diff2))%>%
  dplyr::select(cond,diff)%>%
  add_column(dist='2 - 3+')
diff_loc <- rbind(diff_loc1,diff_loc2)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
diff_loc
write_csv(diff_loc,
          './VWM/output/results/testing/diff_loc_obs.csv')

## col ==================
diff_col1 <- merge(prec_col1,prec_col2)%>%
  mutate(diff1=prec1-prec2)%>%
  dplyr::group_by(cond)%>%
  summarise(diff=mean(diff1))%>%
  dplyr::select(cond,diff)%>%
  add_column(dist='1 - 2')

diff_col2 <- merge(prec_col2,prec_col3)%>%
  mutate(diff2=prec2-prec3)%>%
  dplyr::group_by(cond)%>%
  summarise(diff=mean(diff2))%>%
  dplyr::select(cond,diff)%>%
  add_column(dist='2 - 3+')

diff_col <- rbind(diff_col1,diff_col2)%>%
  mutate(cond = dplyr::recode(cond,"1" = "Both",
                              "2" = "Color",
                              "3" = "Location"))
diff_col
write_csv(diff_col,
          './VWM/output/results/testing/diff_col_obs.csv')

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