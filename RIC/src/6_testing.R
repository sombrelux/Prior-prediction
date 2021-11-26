rm(list=ls())
library(tidyverse)
library(bayestestR)

# core pred of responses ----------
for(i in c(1,5,10,100,200,500)){
  hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_normal_',i,'.csv'))%>%
    add_column(trial_sorted = rep(1:16,6*4))
  dp <- read_csv(paste0('./RIC/output/results/data_prior/dp_normal_',i,'.csv'))%>%
    add_column(model = 'Data prior')
  ggplot(hdi_RITCH,
         mapping = aes(x = trial_sorted)) + 
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high), 
                alpha = 0.35) + 
    geom_segment(aes(x=trial_sorted,xend=trial_sorted,
                     y=CI_low,yend=CI_high,group=ind,
                     col=as.factor(ind)),
                 data=dp)+
    facet_grid(manipulation~choice)+
    labs(x = "Trial", y = "Prop.Option.1",
         title=paste0('SD = ',i,'*sd'))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0('./RIC/output/fig/normal_',i,'.png'),
         width = 6,height = 5)
}

for(i in c(3,15,30,300,600,1500)){
  hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_unif_',i,'.csv'))%>%
    add_column(trial_sorted = rep(1:16,6*4))
  dp <- read_csv(paste0('./RIC/output/results/data_prior/dp_unif_',i,'.csv'))%>%
    add_column(model = 'Data prior')
  ggplot(hdi_RITCH,
         mapping = aes(x = trial_sorted)) + 
    geom_ribbon(aes(ymin = CI_low, 
                    ymax = CI_high), 
                alpha = 0.35) + 
    geom_segment(aes(x=trial_sorted,xend=trial_sorted,
                     y=CI_low,yend=CI_high,group=ind,
                     col=as.factor(ind)),
                 data=dp)+
    facet_grid(manipulation~choice)+
    labs(x = "Trial", y = "Prop.Option.1",
         title=paste0('SD = ',i,'*sd'))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0('./RIC/output/fig/unif_',i,'.png'),
         width = 6,height = 5)
}

# data prior of manipulation effect -----------------
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

