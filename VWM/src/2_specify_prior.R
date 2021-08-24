rm(list=ls())
library(tidyverse)
library(R.matlab)
library(circular)
library(MASS)

# a, b, r -----------
rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}

n <- 10000
b <- rtruncnorm(n,0.11,0.15,0,10000)
median(b)#0.151
sd(b)#0.111
mean(b)#0.167
hist(b)

a <- rtruncnorm(n,-0.3,0.33,0,100000)
median(a)#0.143
sd(a)#0.152
mean(a) #0.182
hist(a)

r <- rbeta(n,1,10)
mean(r)
median(r)
sd(r)
hist(r)

sloc <- rtruncnorm(n,0,9,0,100000)
mean(sloc)
median(sloc)
sd(sloc)
hist(sloc)
# w ----------------
w <- rtruncnorm(n,1,0.2,0,1)
hist(w)

# s_col --------------
wrap = function(angle) { 
  #transform (-2pi,2pi) to (-pi,pi)
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

s <- function(k) sqrt(-2*log(besselI(k,1,expon.scaled = T)/
                               besselI(k,0,expon.scaled = T)))
22*pi/180#0.384
s(7.2)#0.387
s(7.3)#0.385
s(7.4)#0.381

disp_col <- as.numeric(rvonmises(10000,mu=0,kappa=7.3))
sens_col <- wrap(disp_col)
hist(sens_col,freq = F) # very small beyond 1
df_sens <- data.frame(x=1:10000,y=sens_col)
exp_approx <- function(x,s) exp(-s*x)
ggplot(df_sens,aes(x=y))+
  geom_histogram(aes(y=..density..),
                 binwidth = 0.05,
                 alpha=0.7)+
  geom_function(aes(color='s=5'),fun = exp_approx,
                args = list(s=5),xlim=c(0,1.5),
                size=1,linetype = "dashed")+
  geom_function(aes(color='s=4'),fun = exp_approx,
                args = list(s=4),xlim=c(0,1.5),
                size=1,linetype = "dashed")+
  geom_function(aes(color='s=3'),fun = exp_approx,
                args = list(s=3),xlim=c(0,1.5),
                size=1,linetype = "dashed")+
  labs(x='x',y='Density')+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./VWM/output/fig/s_col.svg",
       width = 7, height = 4.75)

# kappa & kappaf -----------
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