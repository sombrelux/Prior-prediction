
## hier ================
indiff_ric$Exp_ind <- rep(0,length(indiff_ric$Exp))
Set_list <- unique(indiff_ric$Exp)
for(i in 1:length(Set_list)){
  indiff_ric$Exp_ind[indiff_ric$Exp==Set_list[i]] <- i
}

parameters <- c('beta_xo','beta_xa','beta_xr',
                'beta_po','beta_pa','beta_pr',
                'beta_to','beta_ta','beta_tr',
                'beta_xo_i','beta_xa_i','beta_xr_i',
                'beta_po_i','beta_pa_i','beta_pr_i',
                'beta_to_i','beta_ta_i','beta_tr_i',
                'sd_i')

data<-list(
  nExp = length(Set_list),Exp = indiff_ric$Exp_ind,
  nTrial=nrow(indiff_ric),N = indiff_ric$N,
  xs = indiff_ric$xs,ts = indiff_ric$ts, ps = indiff_ric$ps,
  xd = indiff_ric$xd,td = indiff_ric$td, pd = indiff_ric$pd,
  xr = indiff_ric$xr,tr = indiff_ric$tr, pr = indiff_ric$pr,
  y = indiff_ric$y)

samples <- stan(file='./RIC/src/2_tuning_priors/fit_RITCH_ric_hier.stan',
                data=data,
                pars=parameters,
                iter = 4000,
                warmup = 2000,
                chains = 4, 
                thin = 4,
                cores = 4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 20))
traceplot(samples,pars=parameters[1:9])

saveRDS(samples,
        './RIC/output/results/fit_prev/RITCH_indiff_ric.rds')

post_stasts <- rstan::summary(samples)
write.csv(post_stasts$summary,
          paste0('./RIC/output/results/fit_prev/RITCH_',
                 i,'.csv'))
png(paste0('./RIC/output/results/fit_prev/RITCH_',
           i,'.png'),
    width = 6, height = 6, units = 'in', res = 300)
par(mar=c(1,1,1,1))
pairs(samples,pars = parameters[1:9])
dev.off()


retr_set <- read_csv("./RIC/data/previous/Retrieval.csv")%>%
  add_column(Paradigm='Choice')%>%
  dplyr::select(Paradigm,Exp,x1,p1,t1,x2,p2,t2,N,y)
all(retr_set$x1<retr_set$x2)
retr_T <- retr_set%>%filter(t2>0,p2==1,p1==1) #delay
all(retr_T$t1<retr_T$t2)
retr_R <- retr_set%>%filter(t2==0,p2<1,t1==0) #risky
all(retr_R$p1>retr_R$p2)
retr_RI <- retr_set%>%filter(t2>0,p2<1) #ric
retr_RI

indiff_set <- read_csv("./RIC/data/previous/Indiff.csv")%>%
  add_column(Paradigm='Indiff',t1=0,p1=1)%>%
  mutate(y=round(N/2))%>%
  rename(x1=Indifferences,x2=Amounts,t2=Delay,p2=Probability)%>%
  dplyr::select(Paradigm,Exp,x1,p1,t1,x2,p2,t2,N,y)

all(indiff_set$x1<indiff_set$x2)
dim(indiff_set)
indiff_T <- indiff_set%>%filter(t2>0,p2==1) #delay
dim(indiff_T)
indiff_R <- indiff_set%>%filter(t2==0,p2<1) #risky
dim(indiff_R)
indiff_RI <- indiff_set%>%filter(t2>0,p2<1) #ric
dim(indiff_RI)

## combine ============

delay_set <- rbind(retr_T,indiff_T)
risky_set <- rbind(retr_R,indiff_R)
ric_set <- rbind(retr_RI,indiff_RI)

dim(delay_set)
dim(risky_set)
dim(ric_set)
unique(ric_set$Exp)

write_csv(delay_set,'./RIC/data/processed/prev_delay.csv')
write_csv(risky_set,'./RIC/data/processed/prev_risky.csv')
write_csv(ric_set,'./RIC/data/processed/prev_ric.csv')

# fit ric set -----------
ric_set <- read_csv('./RIC/data/processed/prev_ric.csv')

ric_set <- ric_set%>%


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
