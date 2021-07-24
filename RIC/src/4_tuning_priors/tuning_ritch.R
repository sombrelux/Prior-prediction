source('./RIC/src/requires.R')
rm(list=ls())

prev_df<-
  readRDS("./RIC/data/processed/prev_df.rds")

ref_choice <- readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")
ref_interv <- data.frame(
  trial=1:nrow(ref_choice),
  lw = ref_choice$theta_lw,
  up = ref_choice$theta_up
)

intertemp_ref <- 
  ref_choice%>%filter(choice=='DvA')
xd = intertemp_ref$x1 - intertemp_ref$x2
range(xd) #-225 -75
xr = 2*(intertemp_ref$x1 - intertemp_ref$x2)/(intertemp_ref$x1 + intertemp_ref$x2)
range(xr) #-1.2 -0.19
td = intertemp_ref$t2 - intertemp_ref$t1
range(td) #4 29
tr = 2*(intertemp_ref$t2 - intertemp_ref$t1)/(intertemp_ref$t1 + intertemp_ref$t2)
range(tr) #0.25 1.9
