rm(list=ls())
library(tidyverse)
pw <- "./RIC/data/previous/Retrieval/"

# Reeck et al., 2017 ---------
load(paste0(pw,'Reeck17_E1.Rdata'))
Reeck17_E1 <- study1_payne%>%
  mutate(SS=dplyr::recode(choice,'ss'=1,'ll'=0),
         t2=dplyr::recode(ll_time,'4 weeks'=4,
                          '6 weeks'=6,
                          '2 weeks'=2),
         t1=dplyr::recode(ss_time,'0 weeks'=0,
                          '2 weeks'=2))%>%
  group_by(ll_amt_val)%>%
  summarise(N=n(),x1=mean(ss_amt_val),
            x2=mean(ll_amt_val),
            t1=mean(t1)/4,
            t2=mean(t2)/4,
            k=sum(SS))%>%
  add_column(Exp='Reeck17_E1',p1=1,p2=1)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
write_csv(Reeck17_E1,paste0(pw,'Reeck17_E1.csv'))

# CG 2016 ----------------
CG_E1 <- read_csv(paste0(pw,'CG16_E1.csv'))%>%
  mutate(k=round(N*(1-LP)))%>%
  add_column(t1=0,p1=1,p2=1,Exp='CG16_E1')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
CG_E2 <- read_csv(paste0(pw,'CG16_E2.csv'))%>%
  mutate(k=round(N*(1-LP)))%>%
  add_column(p1=1,p2=1,Exp='CG16_E2')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
CG_E3 <- read_csv(paste0(pw,'CG16_E3.csv'))%>%
  mutate(k=round(N*(1-LP)))%>%
  add_column(p1=1,p2=1,Exp='CG16_E3')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
CG16 <- rbind(CG_E1,CG_E2,CG_E3)
head(CG16)
write_csv(CG16,paste0(pw,'CG16.csv'))

# Ericson et al., 2015 ---------
# https://osf.io/z9wcj/
# time in weeks
ericson_set <- 
  read_csv(paste0(pw,"Ericson_et_al_2015.csv"))

intertemp_set <- ericson_set%>%
  filter(Condition==3,
         !is.na(LaterOptionChosen))%>%
  mutate(P=1-LaterOptionChosen,
         t1=T1/4,t2=T2/4)%>%
  filter(X1<15000,X2<15000)

all(intertemp_set$X1-intertemp_set$X2<0)
sort(unique(intertemp_set$X1))

group_id<-intertemp_set%>%
  group_by(Subject)%>%
  group_indices()

question_id<-intertemp_set%>%
  group_by(X1,X2,t1,t2)%>%
  group_indices()

intertemp_set<-intertemp_set%>%
  add_column(ID=group_id,Choice=question_id)

group_result<-intertemp_set%>%
  group_by(Choice)%>%
  summarise(x1=mean(X1),x2=mean(X2),
            N = n(),t1=mean(t1),t2=mean(t2),
            k=sum(P))%>%
  add_column(p1=1,p2=1,Exp='Ericson15')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
write_csv(group_result,paste0(pw,'Ericson15.csv'))

# SR 2014 ------------
SR_set1 <- 
  read_csv(paste0(pw,"SR14.csv"))%>%
  mutate(k=round(N*(1-LR)))%>%
  add_column(t1=0,t2=0,p1=1)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
write_csv(SR_set1,paste0(pw,'SR14.csv'))

# SRS 2014 -------------
SRS_E2 <- 
  read_csv(paste0(pw,"SRS14_E2.csv"))%>%
  mutate(P=1/(`odd-LL`+1))%>%
  mutate(k=round(N*P))%>%
  add_column(p1=1,p2=1,Exp='SRS14_E2')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
SRS_E3 <- 
  read_csv(paste0(pw,"SRS14_E3.csv"))%>%
  mutate(P=1/(`odd-LL`+1))%>%
  mutate(k=round(N*P))%>%
  add_column(t1=0,p1=1,p2=1,Exp='SRS14_E3')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
SRS14 <- rbind(SRS_E2,SRS_E3)
SRS14
write_csv(SRS14,paste0(pw,'SRS14.csv'))

# SR 2010 -------------
SR_set2 <- read_csv(paste0(pw,'SR10.csv'))
k1 <- SR_set2$N*SR_set2$o
k1
k <- c(k1[1:8],SR_set2$k[9:11])
Exp <- paste0('SR10_E',SR_set2$Exp)
Exp
SR_set2 <- SR_set2%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,N)%>%
  add_column(Exp,k)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
SR_set2  
write_csv(SR_set2,paste0(pw,'SR10.csv'))

# WC 2005 ------------
WC05 <- read_csv(paste0(pw,'WC05.csv'))%>%
  mutate(k=round(N*prop))
Exp <- paste0('WC05_',WC05$table)
WC05 <- WC05%>%add_column(Exp)%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
WC05
write_csv(WC05,paste0(pw,'WC05.csv'))

# GW 2002,1992 ------------
## Exp in 1992: 20 subj, each 10 times
## Exp in 2002: 7 subj in analysis, each 20 time
GW_set <- read_csv(paste0(pw,'GW2002.csv'))
GW_set <- GW_set%>%
  mutate(k=round(n*o))%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,n,k)
GW_set$Exp <- paste0('GW',GW_set$Exp)
write_csv(GW_set,paste0(pw,'GW02.csv'))

# Erev et al. 2002 -----------
erev_set <- read_csv(paste0(pw,"Erev2002.csv"))
x1 <- erev_set$v1
x2 <- erev_set$v2
x1[erev_set$v1>erev_set$v2] <- erev_set$v2[erev_set$v1>erev_set$v2]
x2[erev_set$v1>erev_set$v2] <- erev_set$v1[erev_set$v1>erev_set$v2]
all(x1<=x2)

p1 <- erev_set$p1
p2 <- erev_set$p2
p1[erev_set$v1>erev_set$v2] <- erev_set$p2[erev_set$v1>erev_set$v2]
p2[erev_set$v1>erev_set$v2] <- erev_set$p1[erev_set$v1>erev_set$v2]
all(p1>=p2)

theta <- erev_set$o
theta[erev_set$v1>erev_set$v2] <- 1-theta[erev_set$v1>erev_set$v2] 

Erev_df <- data.frame(x1,p1,x2,p2,theta)%>%
  mutate(N=38,
         k=round(38*theta))%>%
  add_column(t1=0,t2=0)

Erev_df <- Erev_df%>%
  add_column(Exp='Erev02')%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,k)
head(Erev_df)
write_csv(Erev_df,paste0(pw,'Erev_02.csv'))
