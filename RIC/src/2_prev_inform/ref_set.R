source('./RIC/src/requires.R')
rm(list=ls())

# Experimental design ---------------
choice_set <- 
  read_csv("./RIC/data/processed/choice_set.csv")

range(choice_set$x1) #50-4500
range(choice_set$x2) #50-4750
range(choice_set$t1) #0-66
range(choice_set$t2) #0-66

delay_df <- choice_set%>%
  filter(p1==1,p2==1,choice=='DvA')
range(delay_df$x1) #75-3500
range(delay_df$x2) #200-4750
range(delay_df$x1-delay_df$x2) #-2250~-75
range(delay_df$t1) #1-42
range(delay_df$t2) #5-54
range(delay_df$t1-delay_df$t2) #-29~-4

risky_df <- choice_set%>%
  filter(t1==0,t2==0,choice=='RvA')
range(risky_df$x1) #50-3000
range(risky_df$x2) #250-4750
range(risky_df$x1-risky_df$x2) #-3250~-125
range(risky_df$p1-risky_df$p2) #0.02-0.4

dr_df <- choice_set%>%
  filter(t1==0,p1==1,choice=='DRvA')
range(dr_df$x1) #50-2750
range(dr_df$x2) #75-4750
range(dr_df$x1-dr_df$x2) #-3500~-25
range(dr_df$p1-dr_df$p2) #0.05-0.55
range(dr_df$t2) #2-21
range(dr_df$t1-dr_df$t2) #-21~-2

# Luckman et al., 2018 ----------
## risky =============
Risky_subset <- choice_set%>%
  filter(choice == 'RvA')%>%
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
