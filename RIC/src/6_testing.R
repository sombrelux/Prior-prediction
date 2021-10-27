rm(list=ls())
library(tidyverse)
library(bayestestR)
i <- 100 #5,10,100

# core pred of responses ----------
hdi_RITCH <- read_csv(paste0('./RIC/output/results/core_pred/hdi_RITCH_ind_',i,'.csv'))%>%
  add_column(trial_sorted = rep(1:16,6*4))
ggplot(hdi_RITCH,
       mapping = aes(x = trial_sorted,
                     group=manipulation)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill=manipulation), 
              alpha = 0.35) + 
  facet_wrap(~choice)+
  labs(x = "Trial", y = "Prop.Option.1")

hdi_HD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_HD_ind_',i,'.csv'))
hdi_HD_s <- hdi_HD[match(hdi_RITCH$trial,hdi_HD$trial),]%>%
  add_column(trial_sorted = rep(1:16,6*4))
hdi_MHD <- read_csv(paste0('./RIC/output/results/core_pred/hdi_MHD_ind_',i,'.csv'))
hdi_MHD_s <- hdi_MHD[match(hdi_RITCH$trial,hdi_MHD$trial),]%>%
  add_column(trial_sorted = rep(1:16,6*4))
hdi_PTT <- read_csv(paste0('./RIC/output/results/core_pred/hdi_PTT_ind_',i,'.csv'))
hdi_PTT_s <- hdi_PTT[match(hdi_RITCH$trial,hdi_PTT$trial),]%>%
  add_column(trial_sorted = rep(1:16,6*4))

hdi_prior <- rbind(hdi_HD_s,hdi_MHD_s,hdi_PTT_s,hdi_RITCH)

ggplot(hdi_prior,
       mapping = aes(x = trial_sorted,
                     group=model,
                     col=model)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill = model), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title=paste0('SD = ',i,'*sd'))+
  theme(plot.title = element_text(hjust = 0.5))

# data prior of manipulation effect -----------------
base_ind <- choice_set$manipulation=='Base'
mag_ind <- choice_set$manipulation=='Mag'
cert_ind <- choice_set$manipulation=='Cert'
imm_ind <- choice_set$manipulation=='Imm'

eff_PTT <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
  bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
  bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
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

