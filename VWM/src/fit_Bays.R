source('./VWM/src/requires.R')
rm(list = ls())

Sys.setenv(STAN_NUM_THREADS = 4)
# Fit Bays et al., 2009 ---------------
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
	               iter= 3000, 
	               chains=4, 
	               cores=4,
	               warmup = 2000,
	               seed = 123)
	saveRDS(fit_im,
	        paste0('./VWM/output/results/fit_prev/subj/IM_subj',i,'.rds'))
	rm(list = c('ind_i','data','fit_im'))
	Sys.sleep(20)
}

# Pooled prior distributions -----------------
rm(list=ls())
dir <- getwd()
setwd("./VWM/output/results/fit_prev/subj")
subj_files <- list.files()
posterior_dist <- array(dim=c(4000,12,7))
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[i])
  posterior_dist[,i,] <- as.matrix(samples)
}
dim(posterior_dist)

parameters <- c('a','b','s','r',
                'kappa','kappaf')
post_plots <- list()
for(i in 1:length(parameters)){
  post_i <- as.data.frame(posterior_dist[,,i])%>%
    pivot_longer(cols = everything(),
                 names_to = 'subj',
                 values_to = 'posterior')
  post_plots[[i]] <- ggplot(post_i)+
    geom_density(aes(x=posterior,group=subj,
                     col=subj,fill=subj),
                 alpha=0.1)+
    scale_x_continuous(parameters[i])+
    scale_y_continuous('')+
    theme(axis.text=element_text(size=13),
          axis.title=element_text(size=16),
          strip.text.x = element_text(size = 14),
          legend.position = 'none')
}
setwd(dir)
ggarrange(plotlist=post_plots,nrow=2,ncol=3)
ggsave('./VWM/output/fig/fit_prev/post_subj.svg',
       height=4,width = 8)

rtruncnorm <- function(n,mu,sig,lb){
  ulb <- pnorm(lb,mu,sig)
  u <- runif(n,ulb,1)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 100000
set.seed(1234)
a <- rbeta(n,1,5)
b <- rbeta(n,2,10)
r <- rbeta(n,1,5)
s <- rtruncnorm(n,12,10,0)
kappa <- rtruncnorm(n,10,2,0)
kappaf <- rtruncnorm(n,30,4,18)

post_prior <- list(
  a,b,s,r,kappa,kappaf
)
post_prior_plots <- list()
for(i in 1:length(parameters)){
  param_df_temp <- data.frame(y=post_prior[[i]])
  post_prior_plots[[i]] <- post_plots[[i]]+
    geom_density(aes(x=y),data=param_df_temp)
}
ggarrange(plotlist=post_prior_plots,nrow=2,ncol=3)

ggsave('./VWM/output/fig/fit_prev/post_prior.svg',
       height=4,width = 8)

