rm(list=ls())
library(tidyverse)
library(intervals)

i <- 100
hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_normal_',i,'.csv'))%>%
  add_column(trial_sorted = rep(1:16,6*4))
hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_normal_',i,'.csv'))
hdi_HD_s <- hdi_HD[match(hdi_RITCH$trial,hdi_HD$trial),]%>%
  add_column(trial_sorted = rep(1:16,6*4))
hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_normal_',i,'.csv'))
hdi_MHD_s <- hdi_MHD[match(hdi_RITCH$trial,hdi_MHD$trial),]%>%
  add_column(trial_sorted = rep(1:16,6*4))
hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
hdi_PTT_s <- hdi_PTT[match(hdi_RITCH$trial,hdi_PTT$trial),]%>%
  add_column(trial_sorted = rep(1:16,6*4))
hdi_all <- rbind(hdi_HD_s,hdi_MHD_s,hdi_PTT_s,hdi_RITCH)

# plot -----------------
ggplot(hdi_all,
       mapping = aes(x = trial_sorted,
                     group=model)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill=model), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="100sd")
 
# data priors ---------------------
for(i in c(1,5,10,50,100)){
  hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_normal_',i,'.csv'))%>%
    add_column(trial_sorted = rep(1:16,6*4))
  hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_normal_',i,'.csv'))
  hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_normal_',i,'.csv'))
  hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
  data_prior <- NULL
  
  for(j in 1:nrow(hdi_RITCH)){
    CI_HD <- hdi_HD%>%filter(trial==hdi_RITCH$trial[j])%>%
      select(CI_low,CI_high)
    CI_MHD <- hdi_MHD%>%filter(trial==hdi_RITCH$trial[j])%>%
      select(CI_low,CI_high)
    CI_PTT <- hdi_PTT%>%filter(trial==hdi_RITCH$trial[j])%>%
      select(CI_low,CI_high)
    DP_mat <- Intervals(rbind(CI_HD,CI_MHD,CI_PTT))
    dp_j <- as.data.frame(interval_union(DP_mat))
    colnames(dp_j) <- c('CI_low','CI_high')
    dp_j$trial <- hdi_RITCH$trial[j]
    dp_j$manipulation <- hdi_RITCH$manipulation[j]
    dp_j$choice <- hdi_RITCH$choice[j]
    dp_j$trial_sorted <- hdi_RITCH$trial_sorted[j]
    dp_j$ind <- 1:nrow(dp_j)
    data_prior <- rbind(data_prior,dp_j)
  }
  dim(data_prior)
  write_csv(data_prior,
            paste0('./RIC/output/results/data_prior/dp_normal_',i,'.csv'))
}

# uniform priors ---------------
rm(list=ls())
for(i in c(3,15,30,300,600,1500)){
  hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_unif_',i,'.csv'))%>%
    add_column(trial_sorted = rep(1:16,6*4))
  hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_unif_',i,'.csv'))
  hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_unif_',i,'.csv'))
  hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_unif_',i,'.csv'))
  data_prior <- NULL
  
  for(j in 1:nrow(hdi_RITCH)){
    CI_HD <- hdi_HD%>%filter(trial==hdi_RITCH$trial[j])%>%
      select(CI_low,CI_high)
    CI_MHD <- hdi_MHD%>%filter(trial==hdi_RITCH$trial[j])%>%
      select(CI_low,CI_high)
    CI_PTT <- hdi_PTT%>%filter(trial==hdi_RITCH$trial[j])%>%
      select(CI_low,CI_high)
    DP_mat <- Intervals(rbind(CI_HD,CI_MHD,CI_PTT))
    dp_j <- as.data.frame(interval_intersection(DP_mat))
    colnames(dp_j) <- c('CI_low','CI_high')
    dp_j$trial <- hdi_RITCH$trial[j]
    dp_j$manipulation <- hdi_RITCH$manipulation[j]
    dp_j$choice <- hdi_RITCH$choice[j]
    dp_j$trial_sorted <- hdi_RITCH$trial_sorted[j]
    dp_j$ind <- 1:nrow(dp_j)
    data_prior <- rbind(data_prior,dp_j)
  }
  dim(data_prior)
  write_csv(data_prior,paste0('./RIC/output/results/data_prior/dp_unif_',i,'.csv'))
}
