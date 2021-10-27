rm(list=ls())
library(tidyverse)
library(intervals)
i <- 100 #5,10,100

hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_ind_',i,'.csv'))
hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_ind_',i,'.csv'))
hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_ind_',i,'.csv'))

for(j in hdi_HD$trial){
  CI_HD <- hdi_HD%>%filter(trial==j)%>%
    select(CI_low,CI_high)
  CI_MHD <- hdi_MHD%>%filter(trial==j)%>%
    select(CI_low,CI_high)
  CI_PTT <- hdi_PTT%>%filter(trial==j)%>%
    select(CI_low,CI_high)
  DP_mat <- Intervals(rbind(CI_HD,CI_MHD,CI_PTT))
  data_prior <- 
}
