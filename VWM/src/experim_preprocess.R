source('./VWM/src/requires.R')
rm(list=ls())

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

range(Response) #1~360 
range(color[,1]) #1~360

# Paul Bays' wrap function -> signed difference!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

location_rad <- 2*pi*location/13 #0~2pi
location_dist <- location_rad - location_rad[,1] #-2pi~2pi
Dist <- abs(wrap(location_dist)) #0~pi

color_rad <- 2*pi*color/360
resp_rad <- 2*pi*Response/360

candidate_resp <- 2*pi*(1:360)/360

exp1<-list(
  nPart = length(unique(id)),
  nTrial = length(id),
  ID = id, 
  M = 8,
  N = 360,
  Setsize = Setsize,
  m = color_rad,
  D = Dist,
  X = candidate_resp,
  response = resp_rad
)
saveRDS(exp1,'./VWM/data/processed/OL_exp1.rds')
