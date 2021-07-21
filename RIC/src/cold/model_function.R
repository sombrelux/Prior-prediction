
# HD --------------
hd_theta<-function(x,d,o,a,h,i,s){
  u1<-x[1,]^a/(1+h*(d[1,]+i*o[1,]))
  u2<-x[2,]^a/(1+h*(d[2,]+i*o[2,]))
  theta<-inv.logit(s*(u1-u2))
  return(theta)
}

# MHD ------------------
mhd_theta<-function(x,d,o,a,h_d,s_d,h_r,s_r,c,s){
  v1<-x[1,]^a
  d1<-(1+h_d*d[1,])^s_d
  w1<-(1+h_r*o[1,])^(s_r*x[1,]^c)
  u1<-v1/(d1*w1)
  v2<-x[2,]^a
  d2<-(1+h_d*d[2,])^s_d
  w2<-(1+h_r*o[2,])^(s_r*x[2,]^c)
  u2<-v2/(d2*w2)
  theta<-inv.logit(s*(u1-u2))
  return(theta)
}

# PTT ---------------
ptt_theta<-function(x,d,p,alpha,beta,gamma,R,s){
  v1<-(1-exp(-alpha*x[1,]^(1-beta)))/alpha
  w1<-exp(-(R*d[1,]/x[1,]-log(p[1,]))^gamma)
  u1<-v1*w1
  v2<-(1-exp(-alpha*x[2,]^(1-beta)))/alpha
  w2<-exp(-(R*d[2,]/x[2,]-log(p[2,]))^gamma)
  u2<-v2*w2
  theta<-inv.logit(s*(u1-u2))
  return(theta)
}

# RITCH & its reduced forms --------
ritch_theta<-function(x,d,p,beta_xo,beta_xa,
                      beta_xr,
                      beta_do,beta_da,beta_dr,
                      beta_po,beta_pa,beta_pr){
  xx<-beta_xo*x[1,]+beta_xa*x[2,]+beta_xr*x[3,]
  pp<-beta_po*p[1,]+beta_pa*p[2,]+beta_pr*p[3,]
  dd<-beta_do*d[1,]+beta_da*d[2,]+beta_dr*d[3,]
  theta<-inv.logit(xx+pp-dd)
  return(theta)
}

itch_theta<-function(x,d,
                     beta_o,beta_xa,beta_xr,
                     beta_da,beta_dr){
  xx<-beta_xa*x[1,]+beta_xr*x[2,]
  dd<-beta_da*d[1,]+beta_dr*d[2,]
  theta<-inv.logit(beta_o+xx-dd)
  return(theta)
}

itch_theta_rva<-function(x,p,
                    beta_o,beta_xa,beta_xr,
                    beta_pa,beta_pr){
  xx<-beta_xa*x[1,]+beta_xr*x[2,]
  pp<-beta_pa*p[1,]+beta_pr*p[2,]
  theta<-inv.logit(beta_o+xx+pp)
  return(theta)
}

itch_theta_dvr<-function(d,p,
                    beta_o,beta_da,beta_dr,
                    beta_pa,beta_pr){
  dd<-beta_da*d[1,]+beta_dr*d[2,]
  pp<-beta_pa*p[1,]+beta_pr*p[2,]
  theta<-inv.logit(beta_o-dd+pp)
  return(theta)
}