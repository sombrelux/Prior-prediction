library(tidyverse)
library(bayestestR)
rm(list=ls())
i <- 100 #5,10,100
# data prior of responses ----------
hdi_HD <- read_csv(paste0('./RIC/output/results/data_prior/hdi_HD_ind_',i,'.csv'))
hdi_MHD <- read_csv(paste0('./RIC/output/results/data_prior/hdi_MHD_ind_',i,'.csv'))
hdi_PTT <- read_csv(paste0('./RIC/output/results/data_prior/hdi_PTT_ind_',i,'.csv'))
hdi_dataprior <- rbind(hdi_HD,hdi_MHD,hdi_PTT)

ggplot(hdi_dataprior,
       mapping = aes(x = trial_num,
                     group=model,
                     col=model)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  fill = model), 
              alpha = 0.35) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title=paste0('Data prior: ',i,'*sd'))+
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

