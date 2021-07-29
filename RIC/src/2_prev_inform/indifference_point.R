source('./RIC/src/requires.R')
rm(list=ls())

# Indifference points ------

## Vanderveldt et al., 2015 --------
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

## Yi et al., 2006 ------
Yi_set <- 
  read.csv('./RIC/data/previous/Yi et al_2006.csv')%>%
  mutate(Indifference=Amounts*proportion,
         exp='Yi')%>%
  dplyr::select(exp,Amounts,Delay,Probability,Indifference)

## Gonzelaz & Wu, 1999 =======
GW_set <-
  read.csv('./RIC/data/previous/GW1999.csv')%>%
  add_column(exp='GW')%>%
  dplyr::select(exp,Amounts,Delay,Probability,Indifference)

# Combine --------------------

indiff_set <- rbind(Vand_set,Yi_set,GW_set)
write_csv(indiff_set,
          './RIC/data/processed/indiff_point.csv')
