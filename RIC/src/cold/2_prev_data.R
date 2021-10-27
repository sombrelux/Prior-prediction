rm(list=ls())
library(tidyverse)
CG_E2 <- read_csv("./RIC/data/previous/Retrieval/SRS14_E3.csv")
CG_t <- CG_E2%>%
  mutate(t2=t2/30,SP = 1/(1+`odd-LL`))%>%
  mutate(y = round(N*SP))%>%
  add_column(t1=0,p1=1,p2=1,Exp='SRS14_E3')%>%
  select(Exp,x1,p1,t1,x2,p2,t2,N,y)

write_csv(CG_t,"./RIC/data/previous/Retrieval/SRS14_E3.csv")

