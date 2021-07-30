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
unique_loc
unique_loc/unique_loc[2]
Locations_code <- Locations/unique_loc[2]
locations_rad <- (2*pi)*Locations_code/8
locations_dist <- locations_rad - locations_rad[,1]
dim(locations_dist)
Dist <- round(abs(wrap(locations_dist)),3)
table(Dist[,2])

Colors <- cbind(df_bays$target,
                df_bays$nontarget)

Dist[is.na(Dist)] <- 0
Colors[is.na(Colors)] <- 0

range(df_bays$response) #-pi~pi
apply(Colors, 2,
      function(u) range(u,na.rm = T)) #-pi~pi

nTrial = length(df_bays$subject)
Setsize = df_bays$n.items

M = max(Setsize); N = 180
bins <- seq(-pi, pi, len = N+1) # discretize [-pi,pi] into 180 bins
X <- bins[1:N]

ind_mat <- matrix(rep(0,nTrial*M),ncol = M)
for(i in 1:nTrial) ind_mat[i,1:Setsize[i]] <- 1

x <- cut(df_bays$response, bins,
         labels = F)
range(x)

E <- array(0,dim = c(nTrial,M,N))
for(i in 1:N) E[,,i] <- as.matrix(wrap(X[i]-Colors))

subjID <- unique(df_bays$subject)

bays_data <- list(
  subjID = subjID,
  subjects = df_bays$subject,
  m = Colors,
  N = N, M = M, Setsize = Setsize,
  ind_mat = ind_mat, Dist = Dist,
  E = E, x=x, response = df_bays$response
)
saveRDS(bays_data,'./VWM/data/processed/bays_data.rds')
