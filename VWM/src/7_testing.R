rm(list=ls())
library(tidyverse)

i=10
# resp error ---------
resp_dp <- readRDS('./VWM/output/results/data_prior/mae_err_exp1.rds')
resp_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/mae_err_ci_", i,"_1.csv"))
resp_obs <- read_csv('./VWM/output/results/testing/mae_err_exp4.csv')
resp_dp
resp_obs

# devnt -----------------
nt_dp <- readRDS('./VWM/output/results/data_prior/mae_nt_exp1.rds')
nt_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/mae_nt_ci_", i,"_1.csv"))
nt_obs <- read_csv('./VWM/output/results/testing/mae_nt_exp4.csv')
nt_dp
nt_obs

# precision -------------
## distance of location ===============
dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')
dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_", i,"_1.csv"))%>%
  mutate(dist = str_replace(dist, "Dloc", ""))
dloc_obs <- read_csv('./VWM/output/results/testing/dloc_exp4.csv')

ggplot(dloc_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Data prior'),
                size=1,
                alpha=0.8,
                width = 0.2)+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Core prediction'),
                size=1.2,
                alpha=0.7,
                width = 0.4,
                data =  dloc_ci)+
  geom_point(aes(y=prec,col='Observation'),
             size=2.5,shape=16,
             data=dloc_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between locations',
       y='Precision of non-target items')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.title = element_blank())
ggsave(paste0('./VWM/output/fig/testing/dloc_',i,'_2.png'),
       height=4, width = 8)

## distance of colors ===============
dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')
dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_", i,"_1.csv"))%>%
  mutate(dist = str_replace(dist, "Dcol", ""))
dcol_obs <- read_csv('./VWM/output/results/testing/dcol_exp4.csv')

ggplot(dcol_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Data prior'),
                size=1,
                alpha=0.8,
                width = 0.2)+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Core prediction'),
                size=1.2,
                alpha=0.7,
                width = 0.4,
                data =  dcol_ci)+
  geom_point(aes(y=prec,col='Observation'),
             size=2.5,shape=16,
             data=dcol_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between colors',
       y='Precision of non-target items')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.title = element_blank())
ggsave(paste0('./VWM/output/fig/testing/dcol_',i,'_2.png'),
       height=4, width = 8)

# difference -------------------
## loc ============
diff_loc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_loc_ci_",i,"_1.csv"))
diff_loc_dp <- read_csv('./VWM/output/results/data_prior/diff_loc_ci.csv')
diff_loc_obs <- read_csv('./VWM/output/results/testing/diff_loc_obs.csv')
ggplot(diff_loc_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Data prior'),
                size=1,
                alpha=0.8,
                width = 0.2)+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Core prediction'),
                size=1.2,
                alpha=0.7,
                width = 0.4,
                data = diff_loc_ci)+
  geom_point(aes(y=diff,
                 col='Observation'),
             size=2.5,shape=16,
             data=diff_loc_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='Difference between precisions')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.title = element_blank())

ggsave(paste0('./VWM/output/fig/testing/diff_loc_',i,'_2.png'),
       height=4, width = 8)

## col ================
diff_col_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_col_ci_",i,"_1.csv"))
diff_col_dp <- read_csv('./VWM/output/results/data_prior/diff_col_ci.csv')
diff_col_obs <- read_csv('./VWM/output/results/testing/diff_col_obs.csv')
ggplot(diff_col_dp,aes(x=dist))+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Data prior'),
                size=1,
                alpha=0.8,
                width = 0.2)+
  geom_errorbar(aes(ymin=CI_low,
                    ymax=CI_high,
                    col='Core prediction'),
                size=1.2,
                alpha=0.7,
                width = 0.4,
                data = diff_col_ci)+
  geom_point(aes(y=diff,
                 col='Observation'),
             size=2.5,shape=16,
             data=diff_col_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='Difference between precisions')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.title = element_blank())

ggsave(paste0('./VWM/output/fig/testing/diff_col_',i,'_2.png'),
       height=4, width = 8)
