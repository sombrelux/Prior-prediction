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
sig_beta_xo_list <- c(0.17,0.83, 1.66,8.29)
for(i in 1:4){
 for(sig_beta_xo in sig_beta_xo_list){
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
     add_column(tag = 'Data prior')%>%
     mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                              'RvAD','DvAR','DRvA')),
            manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
   
   hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_normal_',i,
                                '_',sig_beta_xo,'.csv'))%>%
     mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                              'RvAD','DvAR','DRvA')),
            manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))%>%
     add_column(tag = 'RITCH')
   ritch_ind <- match(df_obs$trial,hdi_RITCH$trial)
   hdi_RITCH_s <- hdi_RITCH[ritch_ind,]
   hdi_RITCH_s$trial_sort <- df_obs$trial_sort
   
   ggplot(hdi_all,
          mapping = aes(x = trial_sort)) +
     geom_ribbon(aes(ymin = CI_low, 
                     ymax = CI_high,
                     group = model,
                     fill = tag,
                     col = tag), 
                 alpha = 0.3,
                 key_glyph = draw_key_smooth)+
     geom_point(aes(y=mean,col=tag),data = df_obs)+
     geom_segment(aes(xend = trial_sort,
                      y=CI_low,yend=CI_high,
                      col=tag),
                  size=1,
                  data = hdi_RITCH_s,
                  key_glyph = draw_key_smooth)+
     facet_grid(manipulation~choice)+
     scale_color_manual(values = c(color_hue[3],color_hue[2],color_hue[1]))+
     scale_fill_manual(values = c(color_hue[3]))+
     guides(fill='none')+
     labs(x = "Trial", y = "Prop.Option.1")+
     theme(legend.title=element_blank())
   ggsave(paste0('./RIC/output/fig/testing/resp_normal_',i,'_',sig_beta_xo,'.png'),
          width = 6,height = 5)
 }
}
