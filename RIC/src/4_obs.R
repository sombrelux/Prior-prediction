rm(list=ls())
library(tidyverse)

resp_set<-read_csv("./RIC/data/raw/ResponseData.csv")
pilot_resp <- read_csv("./RIC/data/processed/pilot_resp.csv")
dir.create('./RIC/output/results/data_prior')

resp_set[resp_set==-1] <- 0
check <- resp_set%>%
  dplyr::select(ID,Dom.1:Dom.6)%>%
  mutate(rowsum = rowSums(.[-1]))%>%
  filter(rowsum > 4)%>%
  dplyr::select(ID)

resp_obs <- resp_set%>%
  filter(ID %in% check$ID,!(ID %in% pilot_resp$ID))%>%
  dplyr::select(RvA.1.Base:DRvA.16.Cert)
mean_obs <- colMeans(resp_obs)
trial <- colnames(resp_obs)

# response data ------------
choice <- factor(sapply(strsplit(trial,'\\.'),function(u) u[1]),
                 levels=c('RvA','RvAD',
                          'DvA','DvAR',
                          'DvR','DRvA'))
manipulation <- factor(sapply(strsplit(trial,'\\.'),function(u) u[3]),
                       levels=c('Base','Mag',
                                'Imm','Cert'))
num <- as.numeric(sapply(strsplit(trial,'\\.'),function(u) u[2]))
df_obs <- data.frame(trial=trial, choice=choice,
                     manipulation = manipulation,
                     num = num,mean = mean_obs)
head(df_obs)

df_obs_sort <- df_obs%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))%>%
  group_by(manipulation,choice)%>%
  arrange(mean,.by_group = T)%>%
  add_column(trial_sort = rep(1:16,24))
write_csv(df_obs_sort,'./RIC/data/processed/response.csv')

# manipulation effect -------------------
manip_eff <- df_obs%>%dplyr::select(-trial)%>%
  pivot_wider(id_cols=c('choice','num'),names_from = manipulation,values_from = mean)%>%
  mutate(mag_eff = Mag-Base, imm_eff = Imm-Base, cert_eff = Cert-Base)%>%
  dplyr::select(-(Base:Cert))%>%
  rename(Mag = mag_eff, Imm = imm_eff, Cert = cert_eff)%>%
  pivot_longer(Mag:Cert,names_to = 'manipulation', values_to = 'eff')

write_csv(manip_eff,'./RIC/data/processed/manip_eff.csv')

