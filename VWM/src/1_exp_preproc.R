rm(list=ls())
library(tidyverse)

# Paul Bays' wrap function -> signed difference!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

# exp4 ----------------
Data <- read.table("./VWM/data/raw/mrc.dat")
dim(Data)
head(Data)

id <- Data[,1]
Condition <- Data[,5]
Setsize <- Data[,6]
orientation <- Data[,3*(3:8)]
color <- Data[,3*(3:8)+1]
location <- Data[,3*(3:8)+2]
Response <- Data[, 27]

range(id) #1~21
range(Response) #1~360 
range(location[,1]) #1~13
range(color[,1]) #2~359
range(orientation[,1]) #1~360

location_rad <- 2*pi*location/13 #0~2pi
location_dist <- location_rad - location_rad[,1] #-2pi~2pi
Dloc <- abs(wrap(location_dist)) #0~pi
unique(round(Dloc[,2],3))

color_rad <- 2*pi*color/360
color_dist <- color_rad - color_rad[,1]
Dcol <- abs(wrap(color_dist))
unique(round(Dcol[,2],3))

orientation_rad <- 2*pi*orientation/360
resp_rad <- 2*pi*Response/360
candidate_resp <- 2*pi*(1:360)/360

E <- array(0,dim = c(length(id),6,360))
for(i in 1:360) E[,,i] <- as.matrix(wrap(candidate_resp[i]-orientation_rad))

exp4 <- list(
  nPart = 21,
  ID = id,
  Condition = Condition,
  N = 360,
  Setsize = 6,
  m = orientation_rad,
  Dcol = Dcol, 
  Dloc = Dloc,
  X = candidate_resp,
  E = E,
  response = resp_rad
)
saveRDS(exp4,'./VWM/data/processed/OL_exp4.rds')

# exp1 ------------------
Data <- read.table("./VWM/data/raw/Colorwheel9.dat")
dim(Data)
head(Data)

id <- Data[,1]
Session<-Data[,2] #2
Setsize <- Data[,5]
color <- Data[, c(6,8,10,12,14,16,18,20)]
location <- Data[, c(7,9,11,13,15,17,19,21)]
Response <- Data[, 22]

Trial_present<-Data[,3]
length(unique(Trial_present)) #400
Trial_design<-Data[,4]
length(unique(Trial_design)) #400

range(id) #1~19
range(Response) #1~360 
range(color[,1]) #1~360

location_rad <- 2*pi*location/13 #0~2pi
location_dist <- location_rad - location_rad[,1] #-2pi~2pi
Dist <- abs(wrap(location_dist)) #0~pi
unique(round(Dist[,2],3))

color_rad <- pi*color/180-pi#-pi~pi
resp_rad <- pi*Response/180-pi#-pi~pi

M = max(Setsize); N = 360
bins <- seq(-pi, pi, len = N+1)
X <- bins[2:(N+1)]#1~360->-pi~pi

nTrial <- length(id)
ind_mat <- matrix(rep(0,nTrial*M),ncol = M)
for(i in 1:nTrial) ind_mat[i,1:Setsize[i]] <- 1

E <- array(0,dim = c(nTrial,M,N))
for(i in 1:N) E[,,i] <- as.matrix(wrap(X[i]-color_rad))

exp1<-list(
  nPart = length(unique(id)),
  nTrial = nTrial,
  N = N, M = M,
  ID = id,
  Setsize = Setsize,
  ind_mat = ind_mat, 
  D = Dist, 
  E = E, 
  x = Response
)

saveRDS(exp1,'./VWM/data/processed/OL_exp1.rds')
