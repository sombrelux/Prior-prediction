rm(list=ls())
library(tidyverse)

i=4;Ub_s=20
# resp error ---------
resp_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/mae_err_ci_", i,'_',Ub_s,".csv"))
resp_dp <- readRDS('./VWM/output/results/data_prior/mae_err_exp1.rds')
resp_dp_df <- data.frame(cond = resp_ci$cond,CI_low=rep(resp_dp[1],3),CI_high=rep(resp_dp[2],3))

ggplot(resp_dp_df,aes(x=cond))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.8,
                width = 0.2,
                col='green')+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.7,
                width = 0.2,
                col='red',
                data =  resp_ci)+
  labs(x='Cue conditon',
       y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')
ggsave(paste0('./VWM/output/fig/testing/resp_err_',i,'.png'),
       height=4, width = 4)

# devnt -----------------
nt_dp <- readRDS('./VWM/output/results/data_prior/mae_nt_exp1.rds')
nt_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/mae_nt_ci_", i,'_',Ub_s,".csv"))
nt_dp_df <- data.frame(cond = nt_ci$cond,CI_low=rep(nt_dp[1],3),CI_high=rep(nt_dp[2],3))

ggplot(nt_dp_df,aes(x=cond))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.8,
                width = 0.2,
                col='green')+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.7,
                width = 0.2,
                col='red',
                data =  nt_ci)+
  labs(x='Cue conditon',
       y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')
ggsave(paste0('./VWM/output/fig/testing/nt_resp_',i,'.png'),
       height=4, width = 4)

# distance -------------
## distance of location ===============
dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')
dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_", i,"_20.csv"))%>%
  mutate(dist = str_replace(dist, "Dloc", ""))

ggplot(dloc_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.8,
                width = 0.2,
                col='green')+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.7,
                width = 0.2,
                col='red',
                data =  dloc_ci)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between locations',
       y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')
ggsave(paste0('./VWM/output/fig/testing/dloc_',i,'.png'),
       height=4, width = 8)

## distance of colors ===============
dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')
dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_", i,"_20.csv"))%>%
  mutate(dist = str_replace(dist, "Dcol", ""))

ggplot(dcol_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.8,
                width = 0.2,
                col='green')+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.7,
                width = 0.2,
                col='red',
                data =  dcol_ci)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between colors',
       y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')
ggsave(paste0('./VWM/output/fig/testing/dcol_',i,'.png'),
       height=4, width = 8)

# difference -------------------
## loc ============
diff_loc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_loc_ci_",i,"_20.csv"))
diff_loc_dp <- read_csv('./VWM/output/results/data_prior/diff_loc_ci.csv')
ggplot(diff_loc_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.8,
                width = 0.2,
                col='green')+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.7,
                width = 0.2,
                col='red',
                data = diff_loc_ci)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance contrast',y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

ggsave(paste0('./VWM/output/fig/testing/diff_loc_',i,'.png'),
       height=4, width = 8)

## col ================
diff_col_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_col_ci_",i,"_20.csv"))
diff_col_dp <- read_csv('./VWM/output/results/data_prior/diff_col_ci.csv')
ggplot(diff_col_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.8,
                width = 0.2,
                col='green')+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high),
                size=1,
                alpha=0.7,
                width = 0.2,
                col='red',
                data = diff_col_ci)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance contrast',y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

ggsave(paste0('./VWM/output/fig/testing/diff_col_',i,'.png'),
       height=4, width = 8)

