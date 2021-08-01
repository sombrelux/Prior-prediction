source('./VWM/src/requires.R')
rm(list=ls())

# Paul Bays' wrap function -> signed difference!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

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

# simulate subj20
color_sim <- location_sim <- matrix(nrow=800,ncol=8)

set.seed(1234)
for(i in 1:800){
  color_sim[i,] <- sample(360,8)
  location_sim[i,] <- sample(13,8)
} 
colnames(color_sim) <- colnames(color)
colnames(location_sim) <- colnames(location)
id_sim <- rep(20,800)
setsize_sim <- rep(1:8,each=100)

color <- rbind(color,color_sim)

location <- rbind(location,location_sim)
location_rad <- 2*pi*location/13 #0~2pi
location_dist <- location_rad - location_rad[,1] #-2pi~2pi
Dist <- abs(wrap(location_dist)) #0~pi
unique(round(Dist[,2],3))
view_dist <- 0.5
radius <- tan((5.1/2)/180*pi)*view_dist
smallest_dist <- (2*pi)/13*radius
Dist_m <- Dist/0.483*smallest_dist
  
color_rad <- 2*pi*color/360
resp_rad <- 2*pi*Response/360

candidate_resp <- 2*pi*(1:360)/360

exp1<-list(
  nPart = 20,
  ID = c(id,id_sim), 
  M = 8,
  N = 360,
  Setsize = c(Setsize,setsize_sim),
  m = color_rad,
  D = Dist_m,
  X = candidate_resp,
  response = resp_rad
)
saveRDS(exp1,'./VWM/data/processed/OL_exp1.rds')
