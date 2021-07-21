source('./RIC/src/requires.R')
rm(list=ls())

# HD -------------------
ref_choice <- 
  readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")%>%
  mutate(o1 = (1-p1)/p1,o2 = (1-p2)/p2)

## 1 ======
a_list <- c(0.05,0.1,0.3,0.5,0.7,1,1.5,2,2.5,3)
h_list <- c(0.001,0.01,0.05,0.1,0.5,1,5,10,50,100)
i_list <- c(10,50,100,200,300)
s_list <- c(0.05,0.1,0.3,0.5,0.7,1,2,3,5)

param_grid <- expand.grid(a_list,h_list,i_list,s_list)
flag <- NULL
for(j in 1:nrow(param_grid)){
  a <- param_grid[j,1]
  h <- param_grid[j,2]
  i <- param_grid[j,3]
  s <- param_grid[j,4]
  v1 <- ref_choice$x1^a
  v2 <- ref_choice$x2^a
  w1 <- 1/(1+h*(ref_choice$t1+i*ref_choice$o1))
  w2 <- 1/(1+h*(ref_choice$t2+i*ref_choice$o2))
  theta <- 1/(1+exp(-s*(v1*w1-v2*w2)))
  flag[j] <- all((theta>=ref_choice$theta_lw)&(theta<=ref_choice$theta_up))
}
length(flag)
sum(flag)
param_plaus <- param_grid[flag,]
pairs(param_plaus)

## Erev et al. 2002 ----------------

Erev_df <- 
  readRDS("./RIC/output/results/previous/ref_Erev_2002.rds")%>%
  mutate(o1 = (1-p1)/p1,o2 = (1-p2)/p2)

flag <- NULL
for(j in 1:nrow(param_plaus)){
  a <- param_plaus[j,1]
  h <- param_plaus[j,2]
  i <- param_plaus[j,3]
  s <- param_plaus[j,4]
  v1 <- Erev_df$x1^a
  v2 <- Erev_df$x2^a
  w1 <- 1/(1+h*(i*Erev_df$o1))
  w2 <- 1/(1+h*(i*Erev_df$o2))
  theta <- 1/(1+exp(-s*(v1*w1-v2*w2)))
  flag[j] <- all((theta>=Erev_df$theta_lw)&(theta<=Erev_df$theta_up))
}
sum(flag)
param_plaus2 <- param_plaus[flag,]
param_plaus2 #0.3 0.01  100    2
pairs(param_plaus2)

## Ericson et al., 2015 ==============
Ericson_df <- readRDS("./RIC/output/results/previous/ref_Ericson_2015.rds")
flag <- NULL
for(j in 1:nrow(param_plaus2)){
  a <- param_plaus2[j,1]
  h <- param_plaus2[j,2]
  i <- param_plaus2[j,3]
  s <- param_plaus2[j,4]
  v1 <- Ericson_df$x1^a
  v2 <- Ericson_df$x2^a
  w1 <- 1/(1+h*Ericson_df$t1)
  w2 <- 1/(1+h*Ericson_df$t2)
  theta <- 1/(1+exp(-s*(v1*w1-v2*w2)))
  flag[j] <- all((theta>=Ericson_df$theta_lw)&(theta<=Ericson_df$theta_up))
}
length(flag)
sum(flag)
param_plaus3 <- param_plaus2[flag,]
param_plaus3 #0.3 0.01  100    2
pairs(param_plaus3)

# PTT ---------------
rm(list=ls())
ref_choice <- 
  readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")

## 1 ======
alpha_list <- c(0.1,0.5,1,5,10,50,100)
beta_list <- c(0.9,0.5,0.1,0.05,0,-0.05,-0.1,-0.5,-1,-2,-3)
gamma_list <- c(0.1,0.3,0.5,0.7,1)
R_list <- c(0.1,1,5,10,50,100)
s_list <- c(0.1,0.5,1,5,10,50)

param_grid <- expand.grid(alpha_list,beta_list,gamma_list,R_list,s_list)
flag <- NULL
for(j in 1:nrow(param_grid)){
  alpha <- param_grid[j,1]
  beta <- param_grid[j,2]
  gamma <- param_grid[j,3]
  R <- param_grid[j,4]
  s <- param_grid[j,5]
  
  v1 <- (1-exp(-alpha*ref_choice$x1^(1-beta)))/alpha
  w1 <- exp(-(R*ref_choice$t1/ref_choice$x1-log(ref_choice$p1))^gamma)
  v2 <- (1-exp(-alpha*ref_choice$x2^(1-beta)))/alpha
  w2 <- exp(-(R*ref_choice$t2/ref_choice$x2-log(ref_choice$p2))^gamma)
  
  theta <- 1/(1+exp(-s*(v1*w1-v2*w2)))
  flag[j] <- all((theta>=ref_choice$theta_lw)&(theta<=ref_choice$theta_up))
}
length(flag)
sum(flag)
param_plaus <- param_grid[flag,]

## Erev et al. 2002 ----------------

Erev_df <- 
  readRDS("./RIC/output/results/previous/ref_Erev_2002.rds")

flag <- NULL
for(j in 1:nrow(param_plaus)){
  alpha <- param_plaus[j,1]
  beta <- param_plaus[j,2]
  gamma <- param_plaus[j,3]
  R <- param_plaus[j,4]
  s <- param_plaus[j,5]
  
  v1 <- (1-exp(-alpha*Erev_df$x1^(1-beta)))/alpha
  w1 <- exp(-(-log(Erev_df$p1))^gamma)
  v2 <- (1-exp(-alpha*Erev_df$x2^(1-beta)))/alpha
  w2 <- exp(-(-log(Erev_df$p2))^gamma)
  
  theta <- 1/(1+exp(-s*(v1*w1-v2*w2)))
  flag[j] <- all((theta>=Erev_df$theta_lw)&(theta<=Erev_df$theta_up))
}
length(flag)
sum(flag)
param_plaus2 <- param_plaus[flag,]

## Ericson 2014 =======
Ericson_df <- readRDS("./RIC/output/results/previous/ref_Ericson_2015.rds")
flag <- NULL
for(j in 1:nrow(param_plaus)){
  alpha <- param_plaus[j,1]
  beta <- param_plaus[j,2]
  gamma <- param_plaus[j,3]
  R <- param_plaus[j,4]
  s <- param_plaus[j,5]
  
  v1 <- (1-exp(-alpha*Ericson_df$x1^(1-beta)))/alpha
  w1 <- exp(-(R*Ericson_df$t1/Ericson_df$x1)^gamma)
  v2 <- (1-exp(-alpha*Ericson_df$x2^(1-beta)))/alpha
  w2 <- exp(-(R*Ericson_df$t2/Ericson_df$x2)^gamma)
  
  theta <- 1/(1+exp(-s*(v1*w1-v2*w2)))
  flag[j] <- all((theta>=Ericson_df$theta_lw)&(theta<=Ericson_df$theta_up))
}
length(flag)
sum(flag)
param_plaus2 <- param_plaus[flag,]
param_plaus2 #0.3 0.01  100    2

rm(list=ls())


# MHD -----------------
ref_choice <- 
  readRDS("./RIC/output/results/previous/ref_LDN_2018.rds")%>%
  mutate(o1 = (1-p1)/p1,o2 = (1-p2)/p2)
## 1 =============
a_list <- c(0.05,0.1,0.3,0.5,0.7,1,1.5,2,2.5,3)
hd_list <- c(0.001,0.01,0.05,0.1,0.5,1,5,10)
sd_list <- c(0.001,0.01,0.1,0.5,1,2,5)
hr_list <- c(1,5,10,50)
sr_list <- c(0.001,0.01,0.1,0.5,1,5,10,50)
c_list <- c(0.001,0.01,0.05,0.1,0.5,1,5,10)
s_list <- c(0.05,0.1,0.3,0.5,0.7,1,2,3,5)

param_grid <- expand.grid(a_list,hd_list,sd_list,
                          hr_list,sr_list,c_list,s_list)
flag <- NULL
for(j in 1:nrow(param_grid)){
  a <- param_grid[j,1]
  hd <- param_grid[j,2]
  s_d <- param_grid[j,3]
  hr <- param_grid[j,4]
  sr <- param_grid[j,5]
  cj <- param_grid[j,6]
  s <- param_grid[j,7]
  
  v1 <- ref_choice$x1^a
  v2 <- ref_choice$x2^a
  invw1 <- (1+hr*ref_choice$o1)^(sr*ref_choice$x1^cj)
  invw2 <- (1+hr*ref_choice$o2)^(sr*ref_choice$x2^cj)
  invd1 <- (1+hd*ref_choice$t1)^s_d
  invd2 <- (1+hd*ref_choice$t2)^s_d
  
  u1 <- v1/(invd1*invw1)
  u2 <- v2/(invd2*invw2)
  theta <- 1/(1+exp(-s*(u1-u2)))
  flag[j] <- all((theta>=ref_choice$theta_lw)&(theta<=ref_choice$theta_up))
}
length(flag)
sum(flag)
param_plaus <- param_grid[flag,]
pairs(param_plaus)

## Erev et al. 2002 ==========

Erev_df <- 
  readRDS("./RIC/output/results/previous/ref_Erev_2002.rds")%>%
  mutate(o1 = (1-p1)/p1,o2 = (1-p2)/p2)

flag <- NULL
for(j in 1:nrow(param_plaus)){
  a <- param_plaus[j,1]
  hd <- param_plaus[j,2]
  s_d <- param_plaus[j,3]
  hr <- param_plaus[j,4]
  sr <- param_plaus[j,5]
  cj <- param_plaus[j,6]
  s <- param_plaus[j,7]
  
  v1 <- Erev_df$x1^a
  v2 <- Erev_df$x2^a
  invw1 <- (1+hr*Erev_df$o1)^(sr*Erev_df$x1^cj)
  invw2 <- (1+hr*Erev_df$o2)^(sr*Erev_df$x2^cj)
  
  u1 <- v1/invw1
  u2 <- v2/invw2
  theta <- 1/(1+exp(-s*(u1-u2)))
  flag[j] <- all((theta>=Erev_df$theta_lw)&(theta<=Erev_df$theta_up))
}
sum(flag)
param_plaus2 <- param_plaus[flag,]
param_plaus2 
rm(list=ls())
