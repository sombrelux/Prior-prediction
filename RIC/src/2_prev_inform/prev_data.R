source('./RIC/src/requires.R')
rm(list=ls())

# Luckman et al., 2018 ----------
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")
## risky =============
Risky_subset <- choice_set%>%filter(choice == 'RvA')%>%
  filter(t1==0, t2==0,
         x1<=500,x1>=50,
         x2<=500,x2>=50)%>%
  mutate(EV = x1*p1-x2*p2)%>%
  arrange(EV)
Risky_subset$EV
bins <- c(-120,-50,0)
EV_code <- cut(Risky_subset$EV, bins,
               labels = F)

Risky_subset <- Risky_subset%>%
  add_column(EV_code)%>%
  mutate(theta_lw = ifelse(EV_code==1,0,0.1),
         theta_up = ifelse(EV_code==1,0.9,1))

## intertemporal ================
Intertemp_subset <- choice_set%>%
  filter(choice == 'DvA',
         p1==1, p2==1,
         x1<=500,x1>=50,
         x2<=500,x2>=50,
         t1<55,t2<55)%>%
  mutate(DV = x1*exp(-0.053*t1)-x2*exp(-0.053*t2))%>%
  arrange(DV)
Intertemp_subset$DV

bins <- c(-100,0,26)
DV_code <- cut(Intertemp_subset$DV, bins,
               labels = F)
Intertemp_subset <- Intertemp_subset %>%
  add_column(DV_code)%>%
  mutate(theta_lw = ifelse(DV_code==1,0,0.2),
         theta_up = ifelse(DV_code==1,0.8,1))

## Reference set =======
ref_choice <- Risky_subset%>%
  bind_rows(Intertemp_subset)%>%
  dplyr::select(-c(EV,EV_code,DV,DV_code))
saveRDS(ref_choice,"./RIC/output/results/previous/ref_LDN_2018.rds")

# Erev et al. 2002 ----------------
rm(list=ls())
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
         color = 2-(x1>50&x2>50),
         n=38,
         k=round(38*theta))%>%
  arrange(EV)

Erev_df$EV

ggplot(Erev_df,aes(x=EV,y=theta))+
  geom_point(aes(col=factor(color)))+
  xlim(c(-80,80))+
  ylab('Proportion to choose safer option')+
  scale_color_discrete(name='',
                       labels = c('Both amounts > 50',
                                  'Otherwise'))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./RIC/output/fig/previous/Erev_EV.svg",
       height = 4.75,width = 7)

Erev_df <- Erev_df%>%
  add_column(t1=0,t2=0)%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)
# Yi et al., 2006 ------
Yi_set <- 
  read.csv('./RIC/data/previous/Yi et al_2006.csv')
set.seed(1234)
Yi_df <- 
  data.frame(x1=round(Yi_set$Amouts*Yi_set$proportion),
             p1=1,t1=0, 
             x2=Yi_set$Amouts,
             p2=Yi_set$Probability,
             t2=Yi_set$Delay,
             n=27,
             k=12+sample(2,nrow(Yi_set),replace = T))

# Vanderveldt et al., 2015 --------
mhd_indif <- function(x,d,p,k,h,s_d,sp){
  o <- (1-p)/p
  x/((1+k*d)^s_d*(1+h*o)^sp)
}
## exp1: 51 subj in analysis ======
d2 <- c(0,1,6,24,60)
p2 <- c(0.1,0.25,0.4,0.8,1)
Exp1_dp <- expand.grid(d2,p2)%>%
  filter((Var1!=0)|(Var2!=1))
x1_800 <- mhd_indif(800,Exp1_dp[,1],Exp1_dp[,2],
                    0.167,3.637,0.155,0.65)
x1_40000 <- mhd_indif(40000,Exp1_dp[,1],Exp1_dp[,2],
                      0.14,4.278,0.083,0.683)

Exp1_800 <- data.frame(x1=round(x1_800),
                       p1=1,t1=0,
                       x2=800,p2=Exp1_dp[,2],
                       t2=Exp1_dp[,1],
                       n=51,
                       k=24+sample(2,nrow(Exp1_dp),replace = T))
Exp1_40000 <- data.frame(x1=round(x1_40000),
                       p1=1,t1=0,
                       x2=40000,p2=Exp1_dp[,2],
                       t2=Exp1_dp[,1],
                       n=51,
                       k=24+sample(2,nrow(Exp1_dp),replace = T))
## exp2: 59 subj for each group,  =====
x1_A_800 <- mhd_indif(800,Exp1_dp[,1],Exp1_dp[,2],
                      0.436,3.368,0.22,0.57)
x1_A_40000 <- mhd_indif(40000,Exp1_dp[,1],Exp1_dp[,2],
                      0.122,3.425,0.203,0.654)
x1_B_800 <- mhd_indif(800,Exp1_dp[,1],Exp1_dp[,2],
                      0.323,4.736,0.172,0.493)
x1_B_40000 <- mhd_indif(40000,Exp1_dp[,1],Exp1_dp[,2],
                        0.752,5.474,0.082,0.583)

Exp2_A_800 <- data.frame(x1=round(x1_A_800),
                         p1=1,t1=0,
                         x2=800,p2=Exp1_dp[,2],
                         t2=Exp1_dp[,1],
                         n=59,
                         k=28+sample(2,nrow(Exp1_dp),replace = T))
Exp2_B_800 <- data.frame(x1=round(x1_B_800),
                         p1=1,t1=0,
                         x2=800,p2=Exp1_dp[,2],
                         t2=Exp1_dp[,1],
                         n=59,
                         k=28+sample(2,nrow(Exp1_dp),replace = T))
Exp2_A_40000 <- data.frame(x1=round(x1_A_40000),
                           p1=1,t1=0,
                           x2=40000,p2=Exp1_dp[,2],
                           t2=Exp1_dp[,1],
                           n=59,
                           k=28+sample(2,nrow(Exp1_dp),replace = T))
Exp2_B_40000 <- data.frame(x1=round(x1_B_40000),
                           p1=1,t1=0,
                           x2=40000,p2=Exp1_dp[,2],
                           t2=Exp1_dp[,1],
                           n=59,
                           k=28+sample(2,nrow(Exp1_dp),replace = T))
Vanderveldt_df <- rbind(Exp1_800,
                        Exp2_A_800,
                        Exp2_B_800,
                        Exp1_40000,
                        Exp2_A_40000,
                        Exp2_B_40000)

# Ericson et al., 2015 ------------------
# https://osf.io/z9wcj/
# time in weeks
ericson_set <- read_csv("./RIC/data/previous/Ericson_et_al_2015.csv")

intertemp_set <- ericson_set%>%
  filter(Condition==3,
         !is.na(LaterOptionChosen))%>%
  mutate(SoonerChosen=dplyr::recode(LaterOptionChosen,
                                    '1'=0,'0'=1))

range(intertemp_set$X1) #0.03~10^5
range(intertemp_set$T1) #0~2
range(intertemp_set$T2) #1~5
all(intertemp_set$X1-intertemp_set$X2<0)
sort(unique(intertemp_set$X1))

group_id<-intertemp_set%>%
  group_by(Subject)%>%
  group_indices()

question_id<-intertemp_set%>%
  group_by(X1,X2,T1,T2)%>%
  group_indices()

intertemp_set<-intertemp_set%>%
  add_column(ID=group_id,Choice=question_id)

group_result<-intertemp_set%>%
  group_by(Choice)%>%
  summarise(x1=mean(X1),x2=mean(X2),
            t1=mean(T1)/4,t2=mean(T2)/4,
            n = n(),
            k=sum(SoonerChosen))%>%
  add_column(p1=1,p2=1)%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)

Intertemp_subset2 <- group_result%>%
  mutate(DV = x1*exp(-0.053*t1)-x2*exp(-0.053*t2),
         theta = k/n)%>%
  arrange(DV)
Intertemp_subset2$DV

#ggplot(Intertemp_subset2,aes(x=DV,y=theta))+geom_point()

# combine --------------

prev_df <- rbind(Erev_df,group_result,
                 Yi_df,Vanderveldt_df)

saveRDS(prev_df,
        "./RIC/data/processed/prev_df.rds")
