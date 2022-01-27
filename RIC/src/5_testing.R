rm(list=ls())
library(tidyverse)
dir.create('./RIC/output/results/testing')
dir.create('./RIC/output/fig/testing')

df_obs <- read_csv('./RIC/data/processed/response.csv')%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
df_obs$trial
df_obs$trial_sort

for(i in 1:4){
  hdi_eff_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_eff_',i,'.csv'))
  hdi_eff_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_eff_',i,'.csv'))
  hdi_eff_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_eff_',i,'.csv'))
  hdi_eff_all <- rbind(hdi_eff_HD,hdi_eff_MHD,hdi_eff_PTT)%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
  for(Ub_to in c(0.05,0.1,0.5)){
    hdi_eff_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_eff_',i,
                                     '_',Ub_to,'.csv'))%>%
      mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                               'RvAD','DvAR','DRvA')),
             manipulation = factor(manipulation,levels = c('Mag','Cert','Imm')))
    ggplot(hdi_eff_all,
           mapping = aes(x = trial_num)) +
      geom_ribbon(aes(ymin = CI_low, 
                      ymax = CI_high,
                      group = model,
                      fill = 'Data prior'), 
                  alpha = 0.6)+
      geom_segment(aes(xend = trial_num,
                       y=CI_low,yend=CI_high,
                       col='Core prediction'),
                   alpha = 0.8,
                   size=1,
                   data = hdi_eff_RITCH)+
      geom_hline(yintercept = 0,linetype="dashed")+
      facet_grid(manipulation~choice,scale='free_y')+
      scale_color_manual(values = c("red"))+
      scale_fill_manual(values = c("green"))+
      labs(x = "Trial", y = "Prop.Option.1")+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14),
            strip.text.x = element_text(size = 12),
            legend.position = "none")
    ggsave(paste0('./RIC/output/fig/testing/effect_',i,'_',Ub_to,'.png'),
           height = 6,width = 8)
    
    
  }
}



