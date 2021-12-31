rm(list=ls())
library(tidyverse)

i=4
# precision -------------
## distance of location ===============
dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')
dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_", i,".csv"))%>%
  mutate(dist = str_replace(dist, "Dloc", ""))
dloc_obs <- read_csv('./VWM/output/results/testing/dloc_exp4.csv')

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
                alpha=1,
                width = 0.2,
                data =  dloc_dp)+
  geom_point(aes(y=prec,col='Observation'),data=dloc_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between locations',
       y='Precision of non-target items')+
  scale_size_manual(values=3)


## distance of colors ===============
dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')
dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_", i,".csv"))%>%
  mutate(dist = str_replace(dist, "Dcol", ""))
dcol_obs <- read_csv('./VWM/output/results/testing/dcol_exp4.csv')

ggplot(dcol_ci,aes(x=dist))+
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
                alpha=1,
                width = 0.2,
                data =  dcol_dp)+
  geom_point(aes(y=prec,col='Observation'),data=dcol_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance between colors',
       y='Precision of non-target items')+
  scale_size_manual(values=3)

# difference -------------------
## loc ============
diff_loc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_loc_ci_",i,".csv"))
diff_loc_dp <- read_csv('./VWM/output/results/data_prior/diff_loc_ci.csv')
diff_loc_obs <- read_csv('./VWM/output/results/testing/diff_loc_obs.csv')
ggplot(diff_loc_ci,aes(x=dist))+
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
                data = diff_loc_dp)+
  geom_point(aes(y=diff,
                 col='Observation'),data=diff_loc_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='Difference between precisions')+
  scale_size_manual(values=3)+
  #scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

ggsave('./VWM/output/fig/testing/diff_loc.png',
       height=4, width = 8)
## col ================
diff_col_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_col_ci_",i,".csv"))
diff_col_dp <- read_csv('./VWM/output/results/data_prior/diff_col_ci.csv')
diff_col_obs <- read_csv('./VWM/output/results/testing/diff_col_obs.csv')
ggplot(diff_col_ci,aes(x=dist))+
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
                data = diff_col_dp)+
  geom_point(aes(y=diff,
                 col='Observation'),data=diff_col_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Condition',y='Difference between precisions')+
  scale_size_manual(values=3)+
  #scale_color_manual(values=c('darkred',"#56B4E9"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.title = element_blank())

ggsave('./VWM/output/fig/testing/diff_col.png',
       height=4, width = 8)