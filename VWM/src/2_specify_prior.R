rm(list=ls())
library(tidyverse)
library(R.matlab)
library(circular)
library(MASS)

# w ----------------
rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}

n <- 100000
w <- rbeta(n,3,2)
hist(w)

w <- rtruncnorm(n,1,0.3,0,1)
hist(w)

# kappa & kappaf -----------
wrap = function(angle) { 
  #transform (-2pi,2pi) to (-pi,pi)
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

## vdBerg ===============
dir <- getwd()
setwd('./VWM/data/previous')
subj_files <- list.files()

err_1 <- NULL
file_ind <- grep('E9_',subj_files)
for(i in file_ind){
  subj_dt <- readMat(subj_files[i])$data
  setsize_ind <- subj_dt[[3]]==1
  err_1 <- c(err_1,subj_dt[[1]][setsize_ind])
}
length(err_1)

err_vm1 <- rvonmises(n,mu=0,kappa=15)
err_vm1 <- wrap(as.vector(err_vm1))
kappa_up <- data.frame(
  dataset = 'Pooled',
  error = err_1) %>%
  bind_rows(data.frame(
    dataset = 'von Mises(0, 15)',
    error = err_vm1)
  )%>%
  dplyr::mutate(dataset=factor(dataset,
                level=c('Pooled','von Mises(0, 15)')))
ggplot(kappa_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="",x="Response error",
       y="Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14))
setwd(dir)
ggsave("./VWM/output/fig/kappaf_lw.png",
       height=4,width=6)

d_von_mises<-function(x,pc,kappaf,kappa){
  y<-as.circular(x)
  0.5*dvonmises(y,0,kappaf)+0.5*dvonmises(y,0,kappa)
}
fitdistr(err_1,d_von_mises,
         start = list(kappa=10,kappaf = 20),
         lower = c(0,15),
         upper = c(15,Inf))

pc <- rbernoulli(n)
y_vm1 <- rvonmises(n,mu=0,kappa=6.6)
y_vm1<-wrap(as.vector(y_vm1))
y_vm2 <- rvonmises(n,mu=0,kappa=26.3)
y_vm2<-wrap(as.vector(y_vm2))
y <- pc*y_vm2+(1-pc)*y_vm1

kappaf_up <- data.frame(
  dataset = 'Pooled',
  error = err_1) %>%
  bind_rows(data.frame(
    dataset = 'Mixture',
    error = y)
  )%>%
  dplyr::mutate(dataset=factor(dataset,
         level=c('Pooled','Mixture')))

ggplot(kappaf_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="",
       x="Response error",y="Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14))
ggsave("./VWM/output/fig/kappaf_up.png",
       height=4,width=6)

## kappa_col in OL1 ===============
s <- function(k) sqrt(-2*log(besselI(k,1,expon.scaled = T)/
                               besselI(k,0,expon.scaled = T)))

22*pi/180#0.384
s(7.2)#0.387
s(7.3)#0.385
s(7.4)#0.381
(22-2.7)*pi/180#0.337
s(9.3)#0.338
(22+2.7)*pi/180#0.431
s(6)#0.428

9*pi/180#0.1571
s(41)#0.1571
(9-2.1)*pi/180#0.120
s(70)#0.120
(9+2.1)*pi/180#0.194
s(27)#0.194