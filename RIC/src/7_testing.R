rm(list=ls())
library(tidyverse)
source('./helping func/color.R')

# testing of responses ----------
color_hue <- gg_color_hue(3)

for(i in c(1,5,10,50,100)){
  hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_normal_',i,'.csv'))
  hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_normal_',i,'.csv'))
  hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_normal_',i,'.csv'))
  hdi_all <- rbind(hdi_HD,hdi_MHD,hdi_PTT)%>%
    add_column(tag = 'Data prior')%>%
    mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                             'RvAD','DvAR','DRvA')),
           manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))
  for(sig_beta_xo in c(0.01,0.05,0.1,0.5)){
    hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_normal_',i,'_',sig_beta_xo,'.csv'))%>%
      mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                               'RvAD','DvAR','DRvA')),
             manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))%>%
      add_column(tag = 'RITCH')
    
    ggplot(hdi_all,
           mapping = aes(x = trial_num)) +
      geom_ribbon(aes(ymin = CI_low, 
                      ymax = CI_high,
                      group = model,
                      fill = tag,
                      col = tag), 
                  alpha = 0.3,
                  
                  key_glyph =draw_key_smooth)+ 
      geom_segment(aes(xend=trial_num,
                       y=CI_low,yend=CI_high,
                       col=tag),
                   data=hdi_RITCH,
                   key_glyph =draw_key_smooth)+
      facet_grid(manipulation~choice)+
      labs(x = "Trial", y = "Prop.Option.1")+ 
      scale_color_manual(values = c(color_hue[3],color_hue[1]))+
      scale_fill_manual(values = c(color_hue[3]))+
      facet_grid(manipulation~choice)+
      labs(x = "Trial", y = "Prop.Option.1")+
      guides(fill=FALSE)+
      theme(legend.title=element_blank())
    ggsave(paste0('./RIC/output/fig/testing/resp_normal_',i,'_',sig_beta_xo,'.png'),
           width = 6,height = 5)
  }
}

# testing of manipulation effect -----------------
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

eff_PTT <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bnormal_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bnormal_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
dim(eff_PTT)
hdi_eff_PTT <- hdi(eff_PTT,ci=0.99)
dim(hdi_eff_PTT)
hdi_eff_PTT <- hdi_eff_PTT%>%
  add_column(model = 'PTT',
             manipulation = c(choice_set$manipulation[mag_ind],
                              choice_set$manipulation[cert_ind],
                              choice_set$manipulation[imm_ind]),
             choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                        choice_set$choice[imm_ind]),
             trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                           choice_set$num[imm_ind]),
             trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                       choice_set$trial[imm_ind]))
write_csv(hdi_eff_PTT,'./RIC/output/results/prior_pred/hdi_eff_PTT_ind.csv')

