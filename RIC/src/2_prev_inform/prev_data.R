source('./RIC/src/requires.R')
rm(list=ls())

# Retrievable raw data --------------
## Erev et al. 2002 ----------------
erev_set <- read_csv("./RIC/data/previous/Erev2002.csv")
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

theta<-erev_set$o
theta[erev_set$v1>erev_set$v2] <- 1-theta[erev_set$v1>erev_set$v2] 

Erev_df <- data.frame(x1,p1,x2,p2,theta)%>%
  mutate(EV = x1*p1-x2*p2,
         n=38,
         k=round(38*theta))%>%
  add_column(t1=0,t2=0)%>%
  arrange(EV)

Erev_df$EV

ggplot(Erev_df,aes(x=EV,y=theta))+
  geom_point()+
  xlim(c(-80,80))+
  ylab('Proportion to choose safer option')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./RIC/output/fig/previous/Erev_EV.svg",
       height = 4.75,width = 7)

Erev_df <- Erev_df%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)%>%
  add_column(set='Erev')

## Gonzela & Wu, 2002 ----------------
## Exp in 1992: 20 subj, each 10 times
## Exp in 2002: 7 subj in analysis, each 20 time
GW_set <- read_csv("./RIC/data/previous/GW2002.csv")
GW_set <- GW_set%>%
  mutate(k=round(n*o))%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)%>%
  add_column(set='GW')

## Scholten & Read 2010 ===============
SR_set <- read_csv("./RIC/data/previous/SR2010.csv")
k1 <- SR_set$n*SR_set$o
k <- c(k1[1:8],SR_set$k[9:11])
SR_set <- SR_set%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n)%>%
  add_column(k)%>%
  add_column(set='SR')
## Ericson et al., 2015 ------------------
# https://osf.io/z9wcj/
# time in weeks
ericson_set <- 
  read_csv("./RIC/data/previous/Ericson_et_al_2015.csv")

intertemp_set <- ericson_set%>%
  filter(Condition==3,
         !is.na(LaterOptionChosen))%>%
  mutate(SoonerChosen=dplyr::recode(LaterOptionChosen,
                                    '1'=0,'0'=1),
         t1=T1/4,t2=T2/4)%>%
  filter(X1<20000,X2<20000)

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
            n = n(),t1=mean(t1),t2=mean(t2),
            k=sum(SoonerChosen))%>%
  add_column(p1=1,p2=1)%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)%>%
  add_column(set='Ericson')

# Combine =====================
prev_df <- rbind(Erev_df,GW_set,
                 SR_set,group_result)

saveRDS(prev_df,
        "./RIC/data/processed/prev_df.rds")

