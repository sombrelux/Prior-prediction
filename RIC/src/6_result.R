rm(list=ls())
library(tidyverse)
source('./color.R')

# observations of responses ----------
df_obs <- read_csv('./RIC/data/processed/response.csv')%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
df_obs$trial
df_obs$trial_sort

color_hue <- gg_color_hue(3)

Ub_to_list <- c(0.05,0.1,0.5)
for(i in c(1,5,10)){
  hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_HD_normal_',i,'.csv'))
  HD_ind <- match(df_obs$trial,hdi_HD$trial)
  hdi_HD_s <- hdi_HD[HD_ind,]
  hdi_HD_s$trial_sort <- df_obs$trial_sort
  
  hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_normal_',i,'.csv'))
  MHD_ind <- match(df_obs$trial,hdi_MHD$trial)
  hdi_MHD_s <- hdi_MHD[MHD_ind,]
  hdi_MHD_s$trial_sort <- df_obs$trial_sort
  
  hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_normal_',i,'.csv'))
  PTT_ind <- match(df_obs$trial,hdi_PTT$trial)
  hdi_PTT_s <- hdi_PTT[PTT_ind,]
  hdi_PTT_s$trial_sort <- df_obs$trial_sort
  
  hdi_all <- rbind(hdi_HD_s,hdi_MHD_s,hdi_PTT_s)%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
  
  for(Ub_to in Ub_to_list){
    hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_',i,
                                 '_',Ub_to,'.csv'))%>%
      mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                               'RvAD','DvAR','DRvA')),
             manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
    ritch_ind <- match(df_obs$trial,hdi_RITCH$trial)
    hdi_RITCH_s <- hdi_RITCH[ritch_ind,]
    hdi_RITCH_s$trial_sort <- df_obs$trial_sort
    
    ggplot(hdi_all,
           mapping = aes(x = trial_sort)) +
      geom_ribbon(aes(ymin = CI_low, 
                      ymax = CI_high,
                      group = model,
                      fill = 'Data prior',
                      col = 'Data prior'), 
                  alpha = 0.6,
                  key_glyph = draw_key_smooth)+
      geom_segment(aes(xend = trial_sort,
                       y=CI_low,yend=CI_high,
                       col='Core prediction'),
                   alpha = 0.8,
                   size=1,
                   data = hdi_RITCH_s,
                   key_glyph = draw_key_smooth)+
      geom_point(aes(y=mean,col='Observation'),
                 size=2,shape=16,
                 data = df_obs)+
      facet_grid(manipulation~choice,scale='free_y')+
      scale_color_manual(values = c(color_hue[1],color_hue[2],color_hue[3]))+
      scale_fill_manual(values = c(color_hue[2]))+
      guides(fill='none')+
      labs(x = "Trial", y = "Prop.Option.1")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12),
            legend.title = element_blank())
    
    ggsave(paste0('./RIC/output/fig/result/Response_',i,'_',Ub_to,'.png'),
           height = 6,width = 8)
  }
}

# manipulation effect -----------------
manip_obs <- read_csv('./RIC/data/processed/manip_eff.csv')%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

for(i in c(1,5,10)){
  hdi_eff_HD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_HD_eff_',i,'.csv'))
  hdi_eff_MHD <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_MHD_eff_',i,'.csv'))
  hdi_eff_PTT <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_PTT_eff_',i,'.csv'))
  hdi_eff_all <- rbind(hdi_eff_HD,hdi_eff_MHD,hdi_eff_PTT)%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))

  for(Ub_to in Ub_to_list){
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
                      fill = 'Data prior',
                      col = 'Data prior'), 
                  alpha = 0.6,
                  key_glyph = draw_key_smooth)+
      geom_segment(aes(xend = trial_num,
                       y=CI_low,yend=CI_high,
                       col='Core prediction'),
                   alpha = 0.8,
                   size=1,
                   data = hdi_eff_RITCH,
                   key_glyph = draw_key_smooth)+
      geom_point(aes(x=num,y=eff,col='Observation'),
                 size=2,shape=16,
                 data = manip_obs)+
      geom_hline(yintercept = 0,linetype="dashed")+
      facet_grid(manipulation~choice,scale='free_y')+
      scale_color_manual(values = c(color_hue[1],color_hue[2],color_hue[3]))+
      scale_fill_manual(values = c(color_hue[2]))+
      guides(fill='none')+
      labs(x = "Trial", y = "Prop.Option.1")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12),
            legend.title = element_blank())
    ggsave(paste0('./RIC/output/fig/result/effect_',i,'_',Ub_to,'.png'),
           height = 6,width = 8)
  }
}
