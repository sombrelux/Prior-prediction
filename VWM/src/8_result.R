rm(list=ls())
library(tidyverse)

Ub_s=20
# fig6 ----------------
for(i in 1:4){
  resp_dp <- readRDS('./VWM/output/results/data_prior/mae_err_exp1.rds')
  resp_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/mae_err_ci_", i,'_',Ub_s,".csv"))
  resp_obs <- read_csv('./VWM/output/results/testing/mae_err_exp4.csv')
  resp_dp_df <- data.frame(cond = resp_ci$cond,CI_low=rep(resp_dp[1],3),CI_high=rep(resp_dp[2],3))
  
  p1 <- ggplot(resp_dp_df,aes(x=cond))+
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
    geom_point(aes(y=obs),
               size=2.5,shape=16,
               col='blue',
               data=resp_obs)+
    labs(x='Cue conditon',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12))

  nt_dp <- readRDS('./VWM/output/results/data_prior/mae_nt_exp1.rds')
  nt_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/mae_nt_ci_", i,'_',Ub_s,".csv"))
  nt_obs <- read_csv('./VWM/output/results/testing/mae_nt_exp4.csv')
  nt_dp_df <- data.frame(cond = nt_ci$cond,CI_low=rep(nt_dp[1],3),CI_high=rep(nt_dp[2],3))
  
  p2 <- ggplot(nt_dp_df,aes(x=cond))+
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
    geom_point(aes(y=obs),
               size=2.5,shape=16,
               col='blue',
               data=nt_obs)+
    labs(x='Cue conditon',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = 'none')

  dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')
  dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dloc_ci_", i,"_20.csv"))%>%
    mutate(dist = str_replace(dist, "Dloc", ""))
  dloc_obs <- read_csv('./VWM/output/results/testing/dloc_exp4.csv')
  
  p3 <- ggplot(dloc_dp,aes(x=dist))+
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
    geom_point(aes(y=prec),
               size=2.5,shape=16,
               col='blue',
               data=dloc_obs)+
    facet_wrap(~cond,nrow=1)+
    labs(x='Distance',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = 'none')

  dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')
  dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/dcol_ci_", i,"_20.csv"))%>%
    mutate(dist = str_replace(dist, "Dcol", ""))
  dcol_obs <- read_csv('./VWM/output/results/testing/dcol_exp4.csv')
  
  p4 <- ggplot(dcol_dp,aes(x=dist))+
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
    geom_point(aes(y=prec),
               size=2.5,shape=16,
               col='blue',
               data=dcol_obs)+
    facet_wrap(~cond,nrow=1)+
    labs(x='Distance',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.title = element_blank())
  p5 <- ggarrange(p1, p2, ncol = 2, labels = c("A", "B"))
  ggarrange(p5,p3,p4,
            nrow = 3, 
            labels = c("","C","D"))
  
  ggsave(paste0('./VWM/output/fig/results/priors_',i,'.png'),
         height=8, width = 8)

}

# difference -------------------
## loc ============
diff_loc_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_loc_ci_",i,"_20.csv"))
diff_loc_dp <- read_csv('./VWM/output/results/data_prior/diff_loc_ci.csv')
diff_loc_obs <- read_csv('./VWM/output/results/testing/diff_loc_obs.csv')

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
  geom_point(aes(y=diff),
             size=2.5,shape=16,
             col='blue',
             data=diff_loc_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance contrast',y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

ggsave(paste0('./VWM/output/fig/results/diff_loc_',i,'.png'),
       height=4, width = 8)

## col ================
diff_col_ci <- read_csv(paste0("./VWM/output/results/prior_pred_unfit/diff_col_ci_",i,"_20.csv"))
diff_col_dp <- read_csv('./VWM/output/results/data_prior/diff_col_ci.csv')
diff_col_obs <- read_csv('./VWM/output/results/testing/diff_col_obs.csv')
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
  geom_point(aes(y=diff),
             size=2.5,shape=16,
             col='blue',
             data=diff_col_obs)+
  facet_wrap(~cond,nrow=1)+
  labs(x='Distance contrast',y='MAE')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

ggsave(paste0('./VWM/output/fig/results/diff_col_',i,'.png'),
       height=4, width = 8)
