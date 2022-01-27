rm(list=ls())
library(tidyverse)
library(ggpubr)
dir.create('./VWM/output/fig/testing')
Ub_s=20
resp_dp <- readRDS('./VWM/output/results/data_prior/mae_err_exp1.rds')
resp_dp_df <- data.frame(cond = resp_ci$cond,CI_low=rep(resp_dp[1],3),CI_high=rep(resp_dp[2],3))

for(i in 1:5){
  resp_ci <- read_csv(paste0("./VWM/output/results/prior_pred/mae_err_ci_", i,'_',Ub_s,".csv"))
  
  p1 <-ggplot(resp_dp_df,aes(x=cond))+
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

  nt_dp <- readRDS('./VWM/output/results/data_prior/mae_nt_exp1.rds')
  nt_ci <- read_csv(paste0("./VWM/output/results/prior_pred/mae_nt_ci_", i,'_',Ub_s,".csv"))
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
    labs(x='Cue conditon',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = 'none')

  dloc_dp <- read_csv('./VWM/output/results/data_prior/dloc_exp1.csv')%>%
    mutate(dist=factor(dist,levels = c('0.48','0.97','>0.97')))
  dloc_ci <- read_csv(paste0("./VWM/output/results/prior_pred/dloc_ci_", i,"_20.csv"))%>%
    mutate(dist=factor(dist,levels = c('0.48','0.97','>0.97')))
  
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
    facet_wrap(~cond,nrow=1)+
    labs(x='Distance',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = 'none')

  dcol_dp <- read_csv('./VWM/output/results/data_prior/dcol_exp1.csv')%>%
  mutate(dist=factor(dist,levels = c('0.7','1.4','>1.4')))
  dcol_ci <- read_csv(paste0("./VWM/output/results/prior_pred/dcol_ci_", i,"_20.csv"))%>%
    mutate(dist=factor(dist,levels = c('0.7','1.4','>1.4')))
  
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
    facet_wrap(~cond,nrow=1)+
    labs(x='Distance',
         y='MAE')+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = 'none')

  p5 <- ggarrange(p1, p2, ncol = 2, labels = c("A", "B"))
  ggarrange(p5,p3,p4,
            nrow = 3, 
            labels = c("","C","D"))
  ggsave(paste0('./VWM/output/fig/testing/test_',i,'.png'),
         height=8, width = 8)
}
