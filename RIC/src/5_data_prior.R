rm(list=ls())
library(tidyverse)

# response ----------------
## HD =====================
hdi_hd <- NULL
for(i in c(1,5,10,50,100)){
  hdi_hd_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_normal_',i,'.csv'))
  hdi_hd_i$sigma <- i
  hdi_hd <- rbind(hdi_hd,hdi_hd_i)
}
dim(hdi_hd)

ggplot(hdi_hd,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="HD")

## MHD =====================
rm(list=ls())
hdi_mhd <- NULL
for(i in c(1,5,10,50,100)){
  hdi_mhd_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_normal_',i,'.csv'))
  hdi_mhd_i$sigma <- i
  hdi_mhd <- rbind(hdi_mhd,hdi_mhd_i)
}
dim(hdi_mhd)

ggplot(hdi_mhd,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="MHD")

## PTT =====================
rm(list=ls())
hdi_PTT <- NULL
for(i in c(1,5,10,50,100)){
  hdi_PTT_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
  hdi_PTT_i$sigma <- i
  hdi_PTT <- rbind(hdi_PTT,hdi_PTT_i)
}
dim(hdi_PTT)

ggplot(hdi_PTT,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="PTT")

## Data prior =====================
rm(list=ls())

for(i in c(1,5,10,50,100)){
  hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_normal_',i,'.csv'))
  hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_normal_',i,'.csv'))
  hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
  hdi_all <- rbind(hdi_HD,hdi_MHD,hdi_PTT)
  write_csv(hdi_all,
            paste0('./RIC/output/results/data_prior/response_',i,'.csv'))
  ggplot(hdi_all,
         mapping = aes(x = trial_num,
                       group=model)) + 
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high,
                    fill=model), 
                alpha = 0.35) + 
    facet_grid(manipulation~choice)+
    labs(x = "Trial", y = "Prop.Option.1",
         title=paste0(i,'*sd'))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0('./RIC/output/fig/data_prior_',i,'.png'))
}

# manipulation effect -------------
rm(list=ls())
library(tidyverse)

## HD =====================
hdi_hd <- NULL
for(i in c(1,5,10,50,100)){
  hdi_hd_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_eff_hd_',i,'.csv'))
  hdi_hd_i$sigma <- i
  hdi_hd <- rbind(hdi_hd,hdi_hd_i)
}
dim(hdi_hd)

ggplot(hdi_hd,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="HD")

## MHD =====================
rm(list=ls())
hdi_mhd <- NULL
for(i in c(1,5,10,50,100)){
  hdi_mhd_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_eff_mhd_',i,'.csv'))
  hdi_mhd_i$sigma <- i
  hdi_mhd <- rbind(hdi_mhd,hdi_mhd_i)
}
dim(hdi_mhd)

ggplot(hdi_mhd,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="MHD")

## PTT =====================
rm(list=ls())
hdi_PTT <- NULL
for(i in c(1,5,10,50,100)){
  hdi_PTT_i <- read_csv(paste0('./RIC/output/results/core_pred/hdi_eff_ptt_',i,'.csv'))
  hdi_PTT_i$sigma <- i
  hdi_PTT <- rbind(hdi_PTT,hdi_PTT_i)
}
dim(hdi_PTT)

ggplot(hdi_PTT,
       mapping = aes(x = trial_num,
                     group=sigma)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill= as.factor(sigma)), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="PTT")

## Data prior =================
rm(list=ls())

for(i in c(1,5,10,50,100)){
  hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_eff_hd_',i,'.csv'))
  hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_eff_mhd_',i,'.csv'))
  hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_eff_ptt_',i,'.csv'))
  hdi_all <- rbind(hdi_HD,hdi_MHD,hdi_PTT)
  write_csv(hdi_all,
            paste0('./RIC/output/results/data_prior/effect_',i,'.csv'))
  
  ggplot(hdi_all,
         mapping = aes(x = trial_num,
                       group=model)) + 
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high,
                    fill=model), 
                alpha = 0.35) + 
    facet_grid(manipulation~choice)+
    labs(x = "Trial", y = "Prop.Option.1",
         title=paste0(i,'*sd'))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0('./RIC/output/fig/eff_dp_',i,'.png'))
}
