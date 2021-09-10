rm(list=ls())
library(tidyverse)

# Experimental design ---------------
choice_set <- 
  read_csv("./RIC/data/processed/choice_set.csv")

range(choice_set$x1) #50-4500
range(choice_set$x2) #50-4750
range(choice_set$t1) #0-66
range(choice_set$t2) #0-66

# set the range of amount<=10000, delay<=100
# RIC ------

## Vanderveldt et al., 2015 ========
## exp1: prob X delay
## exp2A: probability fixed in each block, vary delay
## exp2B: delay fixed in each block, vary probability
Vand_Exp1 <- 
  read.csv('./RIC/data/previous/vanderveldt et al_2015_exp1.csv')%>%
  add_column(exp='Vand1')

Vand_Exp2A <- 
  read.csv('./RIC/data/previous/vanderveldt et al_2015_exp2A.csv')%>%
  add_column(exp='Vand2A')

Vand_Exp2B <- 
  read.csv('./RIC/data/previous/vanderveldt et al_2015_exp2B.csv')%>%
  add_column(exp='Vand2B')

Vand_set <- rbind(Vand_Exp1,Vand_Exp2A,Vand_Exp2B)%>%
  add_column(Amounts=800)%>%
  rename(Indifference=indiff)%>%
  dplyr::select(exp,Amounts,Delay,Probability,Indifference)

ggplot(Vand_set,aes(x=Delay,y=Indifference,
                    group=exp,
                    color=exp))+
  geom_point()+
  geom_line()+
  facet_wrap(~Probability,nrow=2)

## Yi et al., 2006 =========
Yi_set <- 
  read.csv('./RIC/data/previous/Indifferences/GMO_E2.csv')%>%
  mutate(#Indifference=Amounts*Indifference,
         exp='GMO_99_E2')%>%
  dplyr::select(exp,Amounts,Probability,Delay,Indifference)
Yi_set
write_csv(Yi_set,
          './RIC/data/previous/Indifferences/GMO_E2.csv')


# IC -------------------
## Gonzelaz & Wu, 1999 =======
# cited by vanderveldt 2015，indiff
GW_set <-
  read.csv('./RIC/data/previous/GW1999.csv')%>%
  add_column(exp='GW99')%>%
  dplyr::select(exp,Amounts,Delay,Probability,Indifference)
  
## Green,Fry, Myerson, 1994 =====
# use young adults
GFM_set <-   read.csv('./RIC/data/previous/GFM_1994.csv')%>%
  add_column(exp='GFM94')%>%
  dplyr::select(exp,Amounts,Delay,Probability,Indifference)

## Raineri and H. Rachlin, 1993 ===
# exp1
RR_set <- read.csv('./RIC/data/previous/RR_1993.csv')%>%
  add_column(exp='RR93')%>%
  dplyr::select(exp,Amounts,Delay,Probability,Indifference)

## Madden et al, 2003 =====


## GMO, 1999 ====
# Exp1, 500 from GM 2004, 10000 from original 
# Exp2, fig from GM, 2004

## Bickel et al, 1999 =========
# never smoker


# Combine ------------------
indiff_set <- rbind(Vand_set,Yi_set)
write_csv(indiff_set,
          './RIC/data/processed/indiff_point.csv')
