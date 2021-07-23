source('./RIC/src/requires.R')
rm(list=ls())

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")

# Luckman et al., 2018 ----------
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
# Ericson et al., 2015 ------------------
# https://osf.io/z9wcj/
# time in weeks
ericson_set <- read_csv("./RIC/data/previous/Ericson_et_al_2015.csv")
range(intertemp_set$X1) #0.03~10^5
range(intertemp_set$T1) #0~2
range(intertemp_set$T2) #1~5
all(intertemp_set$X1-intertemp_set$X2<0)
sort(unique(intertemp_set$X1))

intertemp_set <- ericson_set%>%
  filter(Condition==3,
         !is.na(LaterOptionChosen))%>%
  mutate(SoonerChosen=dplyr::recode(LaterOptionChosen,
                                    '1'=0,'0'=1))

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
  dplyr::select(x1,t1,x2,t2,n,k)

Intertemp_subset2 <- group_result%>%
  mutate(DV = x1*exp(-0.053*t1)-x2*exp(-0.053*t2),
         theta = k/n)%>%
  arrange(DV)
Intertemp_subset2$DV

ggplot(Intertemp_subset2,aes(x=DV,y=theta))+
  geom_point()

# combine --------------
group_result <- group_result%>%
  add_column(p1=1,p2=1)%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)
  
Erev_df <- Erev_df%>%
  add_column(t1=0,t2=0)%>%
  dplyr::select(x1,p1,t1,x2,p2,t2,n,k)

prev_df <- rbind(Erev_df,group_result)

saveRDS(prev_df,
        "./RIC/data/processed/prev_df.rds")
