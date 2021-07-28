source('./RIC/src/requires.R')
rm(list=ls())

choice_set <- 
  read_csv("./RIC/data/processed/choice_set.csv")
Delay_set <- choice_set%>%
  filter(choice=='DvA',p1==1,p2==1)
Risky_set <- choice_set%>%
  filter(choice=='RvA',t1==0,t2==0)

# Intertemporal ----------
#hyperbolic discounting
#V=A/(1+kD), k is right skewed
#natural-log transformed --> log(k) ~ normal

## Kirby et al., 1999 ===========
#x1:11~80 x2:25-85, dollar
#t1:0, t2:1 week ~ 6 months, in days
#k for control
## for all magnitude: mean=0.013, 0.002-0.14
## for x2 in 50-60: 10^-1.85=0.014
## for x2 in 75-85: 10^-2.05=0.0089

## Shenhav et al, 2017 ======
## use kirby questionnaire
## mean=0.0076

# Risky -----------------
## V=A/(1+h*o)


# Both --------------
## Takahashi et al., 2007 =====
## use Yen & day
## median k 0.000489 h 1.0170
## k in month: 0.015, i~ 70

## Bickel et al, 2014 =====
## kirby MCQ, x<85,t1=0, t2<75 days
## k=0.06 

## x<85, p1=1, p2:0.25~0.9
## h=4.83

## Andrade et al, 2012 =====
## kirby MCQ, k=0.016

## Madden MCQ, h=1.41

## Yi et al., 2009 ====
## x=10, k=0.0016, h=0.66
## x=1000, k=0.0004, h=0.91

## Crean et al, 2000 ====
## x=10