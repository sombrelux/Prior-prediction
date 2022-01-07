rm(list=ls())
library(tidyverse)
library(R.matlab)
library(circular)
library(MASS)
library(ggpubr)

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}

n <- 1000000

# kappa & kappaf -----------
## kappa_col in OL1 ===============
k2sd <- function(k) sqrt(-2*log(besselI(k,1,expon.scaled = T)/
                               besselI(k,0,expon.scaled = T)))*180/pi
s1<-k2sd(4:18)
plot(4:18,s1)

s1<-k2sd(30:50)
plot(30:50,s1)

sd2k <- function(s){
  R <- exp(-s^2/2)
  k <- 1/(R^3-4*R^2+3*R)
  if(R<0.85) k  <- -0.4 + 1.39*R + 0.43/(1-R)
  if(R<0.53) k <- 2 * R + R^3 + (5 * R^5)/6
  return(k)
}

sd2k(22.2/180*pi) #7.2
sd2k(9.1/180*pi) #40.1

sd2k((22.2-7.2)/180*pi) #15.1
sd2k((22.2+7.2)/180*pi) #4.4
sd2k((9.1-2.1)/180*pi) #67.5
sd2k((9.1+2.1)/180*pi) #26.7

## location cue =========
### van den Berg et al., 2012 ==============
subj_files <- list.files('./VWM/data/previous')
err_1 <- NULL
file_ind <- grep('E9_',subj_files)
for(i in file_ind){
  subj_dt <- readMat(paste0('./VWM/data/previous/',subj_files[i]))$data
  setsize_ind <- subj_dt[[3]]==1
  err_1 <- c(err_1,subj_dt[[1]][setsize_ind])
}
length(err_1) #1920

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

p1 <- ggplot(kappa_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="",x="Response error",
       y="Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position = 'none')

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

p2 <- ggplot(kappaf_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="",
       x="Response error",y="Density")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

ggarrange(p1,p2,labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave("./VWM/output/fig/kappa_E9.png",
       height=4,width=8)

### Bays 13 ===========
Bays13_dt <- readMat('./VWM/data/previous/Bays13.mat')
# the first cell corresponding to orientation stimuli and the second to colour stimuli. 
Bays13_df <- data.frame(id = Bays13_dt$subject,
                        setsize = Bays13_dt$n.items,
                        targ = Bays13_dt$target[[1]][[1]],
                        resp = Bays13_dt$response[[1]][[1]])
young_subj <- Bays13_dt$age[Bays13_dt$age<=30]
table(Bays13_dt$cond) #1 = 200 ms, 0 = 2000 ms.
Bays13_1 <- Bays13_df%>%
  filter(id %in% young_subj, setsize ==1) #250
err_1 <- wrap(Bays13_1$resp-Bays13_1$targ)

err_vm1 <- rvonmises(n,mu=0,kappa=11)
err_vm1 <- wrap(as.vector(err_vm1))
kappa_up <- data.frame(
  dataset = 'Pooled',
  error = err_1) %>%
  bind_rows(data.frame(
    dataset = 'von Mises(0, 11)',
    error = err_vm1)
  )%>%
  dplyr::mutate(dataset=factor(dataset,
                               level=c('Pooled','von Mises(0, 11)')))

ggplot(kappa_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="",x="Response error",
       y="Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position = 'none')

d_von_mises<-function(x,pc,kappaf,kappa){
  y<-as.circular(x)
  0.5*dvonmises(y,0,kappaf)+0.5*dvonmises(y,0,kappa)
}
fitdistr(err_1,d_von_mises,
         start = list(kappa=10,kappaf = 20),
         lower = c(0,11),
         upper = c(11,Inf))

pc <- rbernoulli(n)
y_vm1 <- rvonmises(n,mu=0,kappa=5.9)
y_vm1<-wrap(as.vector(y_vm1))
y_vm2 <- rvonmises(n,mu=0,kappa=18.3)
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
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

### Bays 14 ================
Bays14_dt <- readMat('./VWM/data/previous/Bays14.mat')
Bays14_df <- data.frame(setsize = Bays14_dt$n.items,
                        err = Bays14_dt$error)
Bays14_1 <- Bays14_df%>%
  filter(setsize ==1) #1798
err_1 <- Bays14_1$err

err_vm1 <- rvonmises(n,mu=0,kappa=14)
err_vm1 <- wrap(as.vector(err_vm1))
kappa_up <- data.frame(
  dataset = 'Pooled',
  error = err_1) %>%
  bind_rows(data.frame(
    dataset = 'von Mises(0, 14)',
    error = err_vm1)
  )%>%
  dplyr::mutate(dataset=factor(dataset,
                               level=c('Pooled','von Mises(0, 14)')))

ggplot(kappa_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="",x="Response error",
       y="Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position = 'none')

d_von_mises<-function(x,pc,kappaf,kappa){
  y<-as.circular(x)
  0.5*dvonmises(y,0,kappaf)+0.5*dvonmises(y,0,kappa)
}
fitdistr(err_1,d_von_mises,
         start = list(kappa=10,kappaf = 20),
         lower = c(0,14),
         upper = c(14,Inf))

pc <- rbernoulli(n)
y_vm1 <- rvonmises(n,mu=0,kappa=5.5)
y_vm1<-wrap(as.vector(y_vm1))
y_vm2 <- rvonmises(n,mu=0,kappa=25.1)
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
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text.x = element_text(size = 12),
        legend.position = 'none')

## color cue =========
#Gorg fig6

# w ----------------

n <- 100000
w <- rbeta(n,3,2)
hist(w)
