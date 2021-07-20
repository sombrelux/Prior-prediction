source('./VWM/src/requires.R')
rm(list = ls())

Sys.setenv(STAN_NUM_THREADS = 4)
# color: Bays et al., 2009 ---------------
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

M = max(Setsize); N = 360
bins <- seq(-pi, pi, len = N+1)
X <- bins[1:N]

ind_mat <- matrix(rep(0,nTrial*M),ncol = M)
for(i in 1:nTrial) ind_mat[i,1:Setsize[i]] <- 1

x <- cut(df_bays$response, bins,
         labels = F)

E <- array(0,dim = c(nTrial,M,N))
for(i in 1:N) E[,,i] <- as.matrix(wrap(X[i]-Colors))

subjID <- unique(df_bays$subject)
parameters <- c('a','b','s','r',
                'kappa','kappaf')
  
for(i in subjID){
  ind_i <- df_bays$subject==i
	data <- list(
	  nTrial = sum(ind_i),
	  N = N, M = M,
	  Setsize = Setsize[ind_i],
	  ind_mat = ind_mat[ind_i,], 
	  D = Dist[ind_i,], 
	  E = E[ind_i,,], 
	  x = x[ind_i]
	)

	fit_im <- stan(file='./VWM/src/fit_im.stan',
	               data=data,
	               pars=parameters,
	               iter= 4000, 
	               chains=4, 
	               thin=2,
	               cores=4,
	               warmup = 2000,
	               seed = 123)
	saveRDS(fit_im,
	        paste0('./VWM/output/results/fit_prev/IM_subj',i,'.rds'))
	rm(list = c('ind_i','data','fit_im'))
	Sys.sleep(20)
}

samples_im <- readRDS('./Output/Fit/IM_subj1.rds')
traceplot(samples_im, pars = parameters[1:6])
pairs(samples_im, pars = parameters[1:6])
neff_ratio(samples_im, pars = parameters[1:6])
print(summary(samples_im,pars = parameters[1:6]))
saveRDS(extract(samples_im),'./Output/Fit/IM_subj3_fit.rds')

post_samples <- readRDS('./Output/Fit/IM_subj3_fit.rds')
xpred<-as.data.frame(post_samples$xpred)
dim(xpred)
range(xpred)
xobs <- x[df_bays$subj==3]
range(xobs)

## centered the pred resp to targ
xpred_cnt <- apply(xpred,1,
                   function(u){
                     u[u-xobs>179] <- u[u-xobs>179]-360
                     u[u-xobs< -180] <- u[u-xobs< -180]+360
                     u
                   })
dim(xpred_cnt)

hdi_im<-hdi(as.data.frame(t(xpred_cnt)),ci=0.99)
hdi_im<-hdi_im%>%
  add_column(true=xobs)%>%
  add_column(
    setsize = Setsize[df_bays$subj==3])%>%
  arrange(setsize,true)%>%
  add_column(trial = 1:sum(df_bays$subj==3))

ggplot(as.data.frame(hdi_im), 
       mapping = aes(x = trial,group=setsize)) + 
  geom_segment(aes(xend=trial,y = CI_low, yend = CI_high), 
              alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = true))+
  facet_wrap(~setsize)

