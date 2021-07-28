#tuning prior 1 based on indifference point
#obtain prior 2
source('./RIC/src/requires.R')
# HD --------
rm(list=ls())

indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")%>%
  filter((Probability!=1)|(Delay!=0))

x1 <- indiff_set$Indifference
x2 <- indiff_set$Amounts
p2 <- indiff_set$Probability
t2 <- indiff_set$Delay
o2=1/p2-1

hdi_hd <- 
  read_csv('./RIC/output/results/tuning_priors/prior_1/hd_indiff_hdi.csv')

# when p2=1, t2!=0,
# when t2 is large, CI_low high --> h need to be smaller
hdi_hd%>%filter(p2==1,t2!=0)
# when p2!=1, t2=0, increase with o to quick 
# h*i need to be smaller
hdi_hd_risk <- hdi_hd%>%filter(p2!=1,t2==0)
plot(hdi_hd_risk$p2,hdi_hd_risk$CI_low)

a <- 0.1
u1 <- x1^a

i <- 50
s <- 5
logh <- -5
u2 <- x2^a/(1+exp(logh)*(t2+i*o2))

hdi_hd2<-hdi_hd%>%add_column(theta=inv.logit(s*(u1-u2)))
# when p2=1, t2!=0, when t2<=24, acceptable
hdi_hd_delay2 <- hdi_hd2%>%filter(p2==1,t2!=0)
# when p2!=1, t2=0, theta <0.5 , increase i
hdi_hd_risk2 <- hdi_hd2%>%filter(p2!=1,t2==0)
plot(hdi_hd_risk2$p2,hdi_hd_risk2$theta)

i <- 80
u2 <- x2^a/(1+exp(logh)*(t2+i*o2))
hdi_hd3<-hdi_hd%>%add_column(theta=inv.logit(u1-u2))
# when p2!=1, t2=0, ci to low, increase i
hdi_hd_risk3 <- hdi_hd3%>%filter(p2!=1,t2==0)
plot(hdi_hd_risk3$p2,hdi_hd_risk3$theta)

# MHD -----------------
rm(list=ls())

indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")%>%
  filter((Probability!=1)|(Delay!=0))

x1 <- indiff_set$Indifference
x2 <- indiff_set$Amounts
p2 <- indiff_set$Probability
o2=1/p2-1
t2 <- indiff_set$Delay

hdi_mhd <- 
  read_csv('./RIC/output/results/tuning_priors/prior_1/mhd_indiff_hdi.csv')

# when p2=1, t2!=0, ci_low high 
# hd or sd need to be smaller
hdi_mhd%>%filter(p2==1,t2!=0)
# when p2!=1, t2=0, increase with o to quick 
# hr or sr or c need to be smaller
hdi_mhd_risk <- hdi_mhd%>%filter(p2!=1,t2==0)
plot(hdi_mhd_risk$p2,hdi_mhd_risk$CI_low)

a <- 0.1
u1 <- x1^a
c <- 0.01
loghr <- 1
s_r <- 0.5
s <- 5
loghd <- -5
s_d <- 0.2
u2 <- x2^a/((1+exp(loghd)*t2)^s_d*(1+exp(loghr)*o2)^(s_r*x2^c))

hdi_mhd2<-hdi_mhd%>%add_column(theta=inv.logit(s*(u1-u2)))
# when p2=1, t2!=0, when t2<=24, acceptable
hdi_mhd_delay2 <- hdi_mhd2%>%filter(p2==1,t2!=0)

c <- 0.01
loghr <- 0.1
s_r <- 0.1
u2 <- x2^a/((1+exp(loghd)*t2)^s_d*(1+exp(loghr)*o2)^(s_r*x2^c))

hdi_mhd3<-hdi_mhd%>%add_column(theta=inv.logit(s*(u1-u2)))
# when p2!=1, t2=0, ci to low, increase i
hdi_mhd_risk3 <- hdi_mhd3%>%filter(p2!=1,t2==0)
plot(hdi_mhd_risk3$p2,hdi_mhd_risk3$theta)

# PTT -------------
rm(list=ls())

indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")%>%
  filter((Probability!=1)|(Delay!=0))

x1 <- indiff_set$Indifference
x2 <- indiff_set$Amounts
p2 <- indiff_set$Probability
o2=1/p2-1
t2 <- indiff_set$Delay

hdi_ptt <- 
  read_csv('./RIC/output/results/tuning_priors/prior_1/ptt_indiff_hdi.csv')

# when p2=1, t2!=0, acceptable
hdi_ptt%>%filter(p2==1,t2!=0)

alpha <- 0.001
beta <- 0.3
gamma <- 0.7
R <- 2
S <- 8

u1 =1-exp(-alpha*x1^(1-beta))
u2 =(1-exp(-alpha*x2^(1-beta)))*exp(-(R*t2/x2-log(p2))^gamma)
hdi_ptt2<-hdi_ptt%>%add_column(theta=inv.logit(S*(u1-u2)))
# when p2=1, t2!=0, when t2<=24, acceptable
hdi_ptt_delay2 <- hdi_ptt2%>%filter(p2==1,t2!=0)

# when p2!=1, t2=0, increase with o to quick 
# hr or sr or c need to be smaller
hdi_ptt_risk <- hdi_ptt2%>%filter(p2!=1,t2==0)
plot(hdi_ptt_risk$p2,hdi_ptt_risk$theta)

# RITCH --------------
rm(list=ls())

hdi_RITCH <- 
  read_csv('./RIC/output/results/tuning_priors/prior_1/RITCH_indiff_hdi.csv')

# DvA, ci too high, to,ta & tr smaller
hdi_ritch_delay <- hdi_RITCH%>%filter(p2==1,t2!=0)
plot(hdi_ritch_delay$t2,hdi_ritch_delay$CI_low)
plot(hdi_ritch_delay$x1-hdi_ritch_delay$x2,hdi_ritch_delay$CI_low)
plot(2*(hdi_ritch_delay$x1-hdi_ritch_delay$x2)/(hdi_ritch_delay$x1+hdi_ritch_delay$x2),
     hdi_ritch_delay$CI_low)

# RvA, when xd << 0, ci low
hdi_ritch_risk <- hdi_RITCH%>%filter(p2!=1,t2==0)
plot(hdi_ritch_risk$p2,hdi_ritch_risk$CI_low)
plot(hdi_ritch_risk$x1-hdi_ritch_risk$x2,hdi_ritch_risk$CI_high)
plot(2*(hdi_ritch_risk$x1-hdi_ritch_risk$x2)/(hdi_ritch_risk$x1+hdi_ritch_risk$x2),
     hdi_ritch_risk$CI_low)

indiff_set <- 
  read_csv("./RIC/data/processed/indiff_point.csv")%>%
  filter((Probability!=1)|(Delay!=0))

x1 <- indiff_set$Indifference
x2 <- indiff_set$Amounts
p2 <- indiff_set$Probability
t2 <- indiff_set$Delay
xs = sign(x1 - x2)
ps = sign(1 - p2)
ts = sign(t2)
xd = x1 - x2
xr = 2*(x1 - x2)/(x1 + x2)
pd = 1 - p2
pr = 2*(1 - p2)/(1 + p2)
td = t2
tr = 2*t2/t2
tr[is.na(tr)] <- 0

beta_xo<-1;beta_to<-0.1;beta_po<-0.1
beta_xa<-0.001;beta_ta<-0.2;beta_pa<-1
beta_xr<-2;beta_tr<-0.01;beta_pr<-2
X = beta_xo*xs+beta_xa*xd+beta_xr*xr;
R = beta_po*ps+beta_pa*pd+beta_pr*pr;
TT = beta_to*ts+beta_ta*td+beta_tr*tr;

hdi_RITCH2<-hdi_RITCH%>%
  add_column(theta=inv.logit(X+R+TT))
# when p2=1, t2!=0, when t2<=24, acceptable
hdi_RITCH_delay2 <- hdi_RITCH2%>%filter(p2==1,t2!=0)
plot(hdi_RITCH_delay2$t2,hdi_RITCH_delay2$theta)
plot(hdi_RITCH_delay2$x1-hdi_RITCH_delay2$x2,hdi_RITCH_delay2$theta)
plot(2*(hdi_RITCH_delay2$x1-hdi_RITCH_delay2$x2)/(hdi_RITCH_delay2$x1+hdi_RITCH_delay2$x2),
     hdi_RITCH_delay2$theta)

# when p2!=1, t2=0, increase with o to quick 
# hr or sr or c need to be smaller
hdi_RITCH_risk2 <- hdi_RITCH2%>%filter(p2!=1,t2==0)
plot(hdi_RITCH_risk2$p2,hdi_RITCH_risk2$theta)
plot(hdi_RITCH_risk2$x1-hdi_RITCH_risk2$x2,hdi_RITCH_risk2$theta)
plot(2*(hdi_RITCH_risk2$x1-hdi_RITCH_risk2$x2)/(hdi_RITCH_risk2$x1+hdi_RITCH_risk2$x2),
     hdi_RITCH_risk2$theta)

