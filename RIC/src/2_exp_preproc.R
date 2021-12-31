rm(list=ls())
library(tidyverse)

# preprocessing -------------
choice_set<-read_csv("./RIC/data/raw/ChoiceSet.csv")

attribute<-choice_set$X1
trial<-colnames(choice_set)[-1]
p1<-as.numeric(choice_set[1,-1])/100
t1<-as.numeric(choice_set[2,-1])
x1<-as.numeric(choice_set[3,-1])
p2<-as.numeric(choice_set[4,-1])/100
t2<-as.numeric(choice_set[5,-1])
x2<-as.numeric(choice_set[6,-1])

choice_set_t<-data.frame(p1,t1,x1,p2,t2,x2)
colnames(choice_set_t)<-c('p1','t1','x1',
                          'p2','t2','x2')
choice_set_t$trial<-trial

choice<-factor(sapply(strsplit(trial,'\\.'),function(u) u[1]),
       levels=c('RvA','RvAD',
                'DvA','DvAR',
                'DvR','DRvA'))
manipulation<-factor(sapply(strsplit(trial,'\\.'),function(u) u[3]),
                     levels=c('Base','Mag',
                              'Imm','Cert'))
num<-as.numeric(sapply(strsplit(trial,'\\.'),function(u) u[2]))
choice_set_t<-choice_set_t%>%
  add_column(choice=choice,manipulation=manipulation,
             num=num)
head(choice_set_t)

write_csv(choice_set_t,"./RIC/data/processed/choice_set.csv")

# Create a pilot set --------
rm(list=ls())
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")
manipulations<-c('Base','Mag','Imm','Cert')

pilot_risky <- choice_set%>%
  filter(choice %in% c('RvA'),
         t1 == 0, t2 == 0,
         p1 %in% seq(0.05,1,0.05),
         p2 %in% seq(0.05,1,0.05),
         x1 %in% seq(50,475,25),
         x2 %in% seq(50,475,25))

pilot_delay <- choice_set%>%
  filter(choice %in% c('DvA'),
         p1 == 1, p2 == 1,
         t1 %in% c(0:6,8,9,12,13,15,18,21,24,27,30,36,42,54),
         t2 %in% c(0:6,8,9,12,13,15,18,21,24,27,30,36,42,54),
         x1 %in% seq(50,475,25),
         x2 %in% seq(50,475,25))

pilot_rd <- choice_set%>%
  filter(choice %in% c('DvR'),
         p1 %in% seq(0.05,1,0.05),
         p2 %in% seq(0.05,1,0.05),
         t1 %in% c(0:6,8,9,12,13,15,18,21,24,27,30,36,42,54),
         t2 %in% c(0:6,8,9,12,13,15,18,21,24,27,30,36,42,54),
         x1 %in% seq(50,475,25),
         x2 %in% seq(50,475,25))

pilot <- rbind(pilot_risky,pilot_delay,pilot_rd)
write_csv(pilot,"./RIC/data/processed/pilot_choice.csv")

resp_set<-read_csv("./RIC/data/raw/ResponseData.csv")
resp_set[resp_set==-1] <- 0
check <- resp_set%>%
  dplyr::select(ID,Dom.1:Dom.6)%>%
  mutate(rowsum = rowSums(.[-1]))%>%
  filter(rowsum > 4)%>%
  dplyr::select(ID)
dim(check)

set.seed(1234)
pilot_subj <- sample(check$ID,20)
pilot_resp <- resp_set%>%
  filter(ID %in% pilot_subj)%>%
  dplyr::select(ID, pilot$trial)
write_csv(pilot_resp, "./RIC/data/processed/pilot_resp.csv")

# response data ------------
resp_obs <- resp_set%>%
  filter(ID %in% check$ID,!(ID %in% pilot_resp$ID))%>%
  dplyr::select(RvA.1.Base:DRvA.16.Cert)
mean_obs <- colMeans(resp_obs)
trial <- colnames(resp_obs)

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

df_obs <- df_obs%>%
  mutate(choice = factor(choice,levels = c('RvA','DvA','DvR',
                                           'RvAD','DvAR','DRvA')),
         manipulation = factor(manipulation,levels = c('Base','Mag','Cert','Imm')))%>%
  group_by(manipulation,choice)%>%
  arrange(mean,.by_group = T)%>%
  add_column(tag = 'Observed',
             trial_sort = rep(1:16,24))
write_csv(df_obs,'./RIC/data/processed/response.csv')
