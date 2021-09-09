rm(list=ls())
library(tidyverse)
# test: core pred vs. data prior -------------
pw <- "./VWM/output/fig/testing/"
prior_file = 'prior_narrow_pool'
dir.create(paste0(pw,prior_file))

## mae of response error ================
mae_err_ci <- read_csv(
  paste0("./VWM/output/results/prior_prediction/",
         prior_file,"/mae_err_ci.csv"))
mae_err_dp <- read_csv(
  "./VWM/output/results/data_prior/resp_err_dp.csv")
mae_err <- merge(mae_err_ci,mae_err_dp,
                 by='cond')

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
  labs(x='Condition',y='MAE of response errors')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())
ggsave(paste0(pw,prior_file,"/resp_err.png"),
       height=4, width = 5)
##  mae of dev_nt ===========
mae_devnt_ci <- read_csv(
    paste0("./VWM/output/results/prior_prediction/",
           prior_file,"/mae_devnt_ci.csv"))
mae_devnt_dp <- read_csv(
  './VWM/output/results/data_prior/dev_nt_dp.csv')
mae_devnt <- merge(mae_devnt_ci,mae_devnt_dp,
                   by='cond')

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

ggsave(paste0(pw,prior_file,"/dev_nt.png"),
       height=4, width = 5)

## mae of devnt vs dist ================
### loc ===========
mae_dloc_ci <- 
  read_csv(paste0("./VWM/output/results/prior_prediction/",
                  prior_file,"/diff_mae_dloc_ci.csv"))
mae_dloc_dp <- read_csv(
  './VWM/output/results/data_prior/dloc_dp.csv')
mae_dloc <- merge(mae_dloc_ci,mae_dloc_dp,
                  by=c('cond','dist'))

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
ggsave(paste0(pw,prior_file,"/dloc.png"),
       height=4, width = 5)

### col ==================
mae_dcol_ci <- 
  read_csv(paste0("./VWM/output/results/prior_prediction/",
                  prior_file,"/diff_mae_dcol_ci.csv"))
mae_dcol_dp <- read_csv(
  './VWM/output/results/data_prior/dcol_dp.csv')

mae_dcol <- merge(mae_dcol_ci,mae_dcol_dp,
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
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)+
  scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())
ggsave(paste0(pw,prior_file,"/dcol.png"),
       height=4, width = 5)
