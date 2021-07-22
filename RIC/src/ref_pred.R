source('./RIC/src/requires.R')
rm(list=ls())

ref_choice <- readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")

# HD -------------
data<-list(
  nTrial=nrow(ref_choice),
  x1=ref_choice$x1,
  x2=ref_choice$x2,
  t1=ref_choice$t1,
  t2=ref_choice$t2,
  o1=1/ref_choice$p1-1,
  o2=1/ref_choice$p2-1)
parameters <- 'ypred'
samples <- stan(
  file='./RIC/src/prior_1_HD.stan',
  data=data,pars=parameters,iter = 500,warmup = 0,
  seed = 123, algorithm="Fixed_param")
