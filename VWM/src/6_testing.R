rm(list=ls())
library(tidyverse)

# plot CI of error vs dist -------------
i=5; sig_s=5; a_w=1; b_w=1

## distance of location ===============
dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')
dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred/dloc_ci_",
                           i,"_",sig_s,"_",a_w,"_",b_w,".csv"))%>%
  mutate(Dist = str_replace(dist, "Dloc", ""))

ggplot(dloc_ci,aes(x=Dist,ymin=CI_low,
                   ymax=CI_high))+
  geom_errorbar(aes(col='Core prediction'),
                size=1.2,
                alpha=1,
                width = 0.2)+
  geom_errorbar(aes(col='Data prior'),
                size=1.2,
                alpha=1,
                width = 0.2,
                data =  dloc_dp)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between locations',
       y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)


## distance of colors ===============
dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')
dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred/dcol_ci_",
                           i,"_",sig_s,"_",a_w,"_",b_w,".csv"))%>%
  mutate(Dist = str_replace(dist, "Dcol", ""))

ggplot(dcol_ci,aes(x=Dist,ymin=CI_low,
                   ymax=CI_high))+
  geom_errorbar(aes(col='Core prediction'),
                size=1.2,
                alpha=1,
                width = 0.2)+
  geom_errorbar(aes(col='Data prior'),
                size=1.2,
                alpha=1,
                width = 0.2,
                data =  dcol_dp)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between colors',
       y='MAE of deviations from non-targets')+
  scale_size_manual(values=3)

