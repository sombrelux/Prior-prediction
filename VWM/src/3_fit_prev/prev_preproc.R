source('./VWM/src/requires.R')
rm(list = ls())

# Preprocess Bays et al., 2009 ---------------
df_bays <- readMat('./VWM/data/previous/bays_data.mat')

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

Locations <- cbind(df_bays$target.pos,
                   df_bays$nontarget.pos)
unique_loc<-unique(Locations[,1])
sort(unique_loc)
unique_loc/sort(unique_loc)[2]
Locations_code <- Locations/sort(unique_loc)[2]
locations_rad <- (2*pi)*Locations_code/8
locations_dist <- locations_rad - locations_rad[,1]
dim(locations_dist)
Dist <- round(abs(wrap(locations_dist)),3)
table(Dist[,2])
view_dist <- 0.6
radius <- tan(4.5/180*pi)*view_dist
smallest_dist <- (2*pi)/8*radius
Dist_m <- Dist*smallest_dist/0.785

Colors <- cbind(df_bays$target,
                df_bays$nontarget)

Dist_m[is.na(Dist_m)] <- 0
Colors[is.na(Colors)] <- 0

range(df_bays$response) #-pi~pi
apply(Colors, 2,
      function(u) range(u,na.rm = T)) #-pi~pi

nTrial = length(df_bays$subject)
Setsize = df_bays$n.items

M = max(Setsize); N = 180
bins <- seq(-pi, pi, len = N+1) # discretize [-pi,pi] into 180 bins
X <- (bins[1:N]+bins[2:(N+1)])/2

ind_mat <- matrix(rep(0,nTrial*M),ncol = M)
for(i in 1:nTrial) ind_mat[i,1:Setsize[i]] <- 1
dim(ind_mat)

x <- cut(df_bays$response, bins,
         labels = F)
range(x)

E <- array(0,dim = c(nTrial,M,N))
for(i in 1:N) E[,,i] <- as.matrix(wrap(X[i]-Colors))

subjID <- unique(df_bays$subject)

bays_data <- list(
  subjID = subjID,
  subjects = df_bays$subject,
  m = Colors, X=X,
  N = N, M = M, Setsize = Setsize,
  ind_mat = ind_mat, Dist = Dist_m,
  E = E, x=x, response = df_bays$response
)
saveRDS(bays_data,'./VWM/data/processed/bays_data.rds')

# Preprocess vdBerg et al., 2012 ---------
rm(list=ls())

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
vdBerg12 <- read.table('./VWM/data/previous/vdBerg2012.dat')
names(vdBerg12) <- c("id", "trial", "exp", "setsize", 
                     "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                     "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8",
                     "response")
vdBerg12 <- vdBerg12%>%filter(exp==3)
Locations <- vdBerg12%>%dplyr::select(loc1:loc8)
unique(Locations[,1])
locations_rad <- (2*pi)*Locations/8
locations_dist <- locations_rad - locations_rad[,1]
Dist <- round(abs(wrap(locations_dist)),3)
table(Dist[,2])
view_dist <- 0.6
radius <- tan(4.5/180*pi)*view_dist
smallest_dist <- (2*pi)/8*radius
Dist_m <- Dist*smallest_dist/0.785

Colors_ind <- vdBerg12%>%dplyr::select(stim1:stim8)/2-1
range(Colors_ind) #0~179
Colors <- 2*Colors_ind/180*pi-pi 
#-pi~pi, 0=-3.14,179=3.10
dim(Colors) #11232 8
range(Colors)

nTrial = length(vdBerg12$id)
Setsize = vdBerg12$setsize

M = max(Setsize); N = 180
bins <- seq(-pi, pi, len = N+1) # discretize [-pi,pi] into 180 bins
X <- bins[1:N] #X[1]=-3.14,X[180]=3.10

ind_mat <- matrix(rep(0,nTrial*M),ncol = M)
for(i in 1:nTrial) ind_mat[i,1:Setsize[i]] <- 1
dim(ind_mat)

range(vdBerg12$response) #2~360
x <- vdBerg12$response/2
range(x) #1~180

E <- array(0,dim = c(nTrial,M,N))
for(i in 1:N) E[,,i] <- as.matrix(wrap(X[i]-Colors))

subjID <- unique(vdBerg12$id) #1~13

vdBerg_data <- list(
  subjID = subjID,
  subjects = vdBerg12$id,
  m = Colors, X=X,
  N = N, M = M, Setsize = Setsize,
  ind_mat = ind_mat, Dist = Dist_m,
  E = E, x=x
)
saveRDS(vdBerg_data,'./VWM/data/processed/vdBerg_data.rds')
