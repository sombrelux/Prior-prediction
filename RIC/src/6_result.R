rm(list=ls())
library(tidyverse)
library(ggpubr)
df_obs <- read_csv('./RIC/data/processed/response.csv')%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
df_obs$trial
df_obs$trial_sort

# fig 3 ----------
i=1;Ub_to=0.05

manip_obs <- read_csv('./RIC/data/processed/manip_eff.csv')%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

hdi_eff_HD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_HD_eff_',i,'.csv'))
hdi_eff_MHD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'.csv'))
hdi_eff_PTT <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_eff_',i,'.csv'))
hdi_eff_all <- rbind(hdi_eff_HD,hdi_eff_MHD,hdi_eff_PTT)%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_',i,
                                 '_',Ub_to,'.csv'))%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
ggplot(hdi_eff_all,
       mapping = aes(x = trial_num)) +
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  group = model,
                  fill = 'Data prior'#,col = 'Data prior'
  ), 
  alpha = 0.6)+
  geom_segment(aes(xend = trial_num,
                   y=CI_low,yend=CI_high,
                   col='Core prediction'),
               alpha = 0.8,
               size=1,
               data = hdi_eff_RITCH)+
  geom_point(aes(x=num,y=eff,col='Observation'),
             size=2,shape=16,
             data = manip_obs)+
  geom_hline(yintercept = 0,linetype="dashed")+
  facet_grid(manipulation~choice,scale='free_y')+
  scale_color_manual(values = c("red","blue"))+
  scale_fill_manual(values = c("green"))+
  labs(x = "Trial", y = "Prop.Option.1")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = "none")
ggsave(paste0('./RIC/output/fig/result/effect_',i,'_',Ub_to,'.png'),
       height = 6,width = 8)

# appendix 1 ----------------
## i=1 ===============
manip_obs <- read_csv('./RIC/data/processed/manip_eff.csv')%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

hdi_eff_HD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_HD_eff_',i,'.csv'))
hdi_eff_MHD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'.csv'))
hdi_eff_PTT <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_eff_',i,'.csv'))
hdi_eff_all <- rbind(hdi_eff_HD,hdi_eff_MHD,hdi_eff_PTT)%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

Ub_to = 0.1
hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_',i,
                                 '_',Ub_to,'.csv'))%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

p1 <- ggplot(hdi_eff_all,
       mapping = aes(x = trial_num)) +
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  group = model,
                  fill = 'Data prior'#,col = 'Data prior'
  ), 
  alpha = 0.6)+
  geom_segment(aes(xend = trial_num,
                   y=CI_low,yend=CI_high,
                   col='Core prediction'),
               alpha = 0.8,
               size=1,
               data = hdi_eff_RITCH)+
  geom_point(aes(x=num,y=eff,col='Observation'),
             size=2,shape=16,
             data = manip_obs)+
  geom_hline(yintercept = 0,linetype="dashed")+
  facet_grid(manipulation~choice,scale='free_y')+
  scale_color_manual(values = c("red","blue"))+
  scale_fill_manual(values = c("green"))+
  labs(x = "Trial", y = "Prop.Option.1")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = "none")

Ub_to = 0.5
hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_',i,
                                 '_',Ub_to,'.csv'))%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

p2 <- ggplot(hdi_eff_all,
             mapping = aes(x = trial_num)) +
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  group = model,
                  fill = 'Data prior'#,col = 'Data prior'
  ), 
  alpha = 0.6)+
  geom_segment(aes(xend = trial_num,
                   y=CI_low,yend=CI_high,
                   col='Core prediction'),
               alpha = 0.8,
               size=1,
               data = hdi_eff_RITCH)+
  geom_point(aes(x=num,y=eff,col='Observation'),
             size=2,shape=16,
             data = manip_obs)+
  geom_hline(yintercept = 0,linetype="dashed")+
  facet_grid(manipulation~choice,scale='free_y')+
  scale_color_manual(values = c("red","blue"))+
  scale_fill_manual(values = c("green"))+
  labs(x = "Trial", y = "Prop.Option.1")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = "none")
ggarrange(p1,p2,ncol=2,nrow=1,labels = c('A','B'))
ggsave('./RIC/output/fig/result/S11.png',
       height = 6,width = 16)

## other i ==============
for(i in 2:4){
  manip_obs <- read_csv('./RIC/data/processed/manip_eff.csv')%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
  
  hdi_eff_HD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_HD_eff_',i,'.csv'))
  hdi_eff_MHD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'.csv'))
  hdi_eff_PTT <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_eff_',i,'.csv'))
  hdi_eff_all <- rbind(hdi_eff_HD,hdi_eff_MHD,hdi_eff_PTT)%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
  
  Ub_to = 0.05
  hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_',i,
                                   '_',Ub_to,'.csv'))%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
  
  p1 <- ggplot(hdi_eff_all,
               mapping = aes(x = trial_num)) +
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high,
                    group = model,
                    fill = 'Data prior'#,col = 'Data prior'
    ), 
    alpha = 0.6)+
    geom_segment(aes(xend = trial_num,
                     y=CI_low,yend=CI_high,
                     col='Core prediction'),
                 alpha = 0.8,
                 size=1,
                 data = hdi_eff_RITCH)+
    geom_point(aes(x=num,y=eff,col='Observation'),
               size=2,shape=16,
               data = manip_obs)+
    geom_hline(yintercept = 0,linetype="dashed")+
    facet_grid(manipulation~choice,scale='free_y')+
    scale_color_manual(values = c("red","blue"))+
    scale_fill_manual(values = c("green"))+
    labs(x = "Trial", y = "Prop.Option.1")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = "none")
  
  Ub_to = 0.1
  hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_',i,
                                   '_',Ub_to,'.csv'))%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
  
  p2 <- ggplot(hdi_eff_all,
               mapping = aes(x = trial_num)) +
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high,
                    group = model,
                    fill = 'Data prior'#,col = 'Data prior'
    ), 
    alpha = 0.6)+
    geom_segment(aes(xend = trial_num,
                     y=CI_low,yend=CI_high,
                     col='Core prediction'),
                 alpha = 0.8,
                 size=1,
                 data = hdi_eff_RITCH)+
    geom_point(aes(x=num,y=eff,col='Observation'),
               size=2,shape=16,
               data = manip_obs)+
    geom_hline(yintercept = 0,linetype="dashed")+
    facet_grid(manipulation~choice,scale='free_y')+
    scale_color_manual(values = c("red","blue"))+
    scale_fill_manual(values = c("green"))+
    labs(x = "Trial", y = "Prop.Option.1")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = "none")
  
  Ub_to = 0.5
  hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_',i,
                                   '_',Ub_to,'.csv'))%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
  
  p3 <- ggplot(hdi_eff_all,
               mapping = aes(x = trial_num)) +
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high,
                    group = model,
                    fill = 'Data prior'#,col = 'Data prior'
    ), 
    alpha = 0.6)+
    geom_segment(aes(xend = trial_num,
                     y=CI_low,yend=CI_high,
                     col='Core prediction'),
                 alpha = 0.8,
                 size=1,
                 data = hdi_eff_RITCH)+
    geom_point(aes(x=num,y=eff,col='Observation'),
               size=2,shape=16,
               data = manip_obs)+
    geom_hline(yintercept = 0,linetype="dashed")+
    facet_grid(manipulation~choice,scale='free_y')+
    scale_color_manual(values = c("red","blue"))+
    scale_fill_manual(values = c("green"))+
    labs(x = "Trial", y = "Prop.Option.1")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          strip.text.x = element_text(size = 12),
          legend.position = "none")
  ggarrange(p1,p2,p3,ncol=2,nrow=2,labels = c('A','B','C'))
  ggsave(paste0('./RIC/output/fig/result/S1',i,'.png'),
         height = 16,width = 16)
}
