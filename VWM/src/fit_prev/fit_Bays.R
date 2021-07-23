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
                'kappa','delta')

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


	fit_im <- stan(file='./VWM/src/fit_prev/fit_im_3.stan',
	               data=data,
	               pars=parameters,
	               chains=4, 
	               cores=4,
	               seed = 123)
	saveRDS(fit_im,
	        paste0('./VWM/output/results/fit_prev/im_3/subj',i,'.rds'))
	rm(list = c('ind_i','data','fit_im'))
	Sys.sleep(20)
}

# Pooled prior distributions -----------------
rm(list=ls())
dir <- getwd()
setwd("./VWM/output/results/fit_prev/im_3")
subj_files <- list.files()
posterior_dist <- array(dim=c(4000,12,6))
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[i])
  posterior <- as.matrix(samples)
  posterior_kappaf <- posterior[,5]+
    posterior[,6]
  posterior_dist[,i,] <- cbind(posterior[,1:5],
                               posterior_kappaf)
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
    scale_y_continuous('')
}

# post to prior -----------
rtruncnorm <- function(n,mu,sig,lb){
  ulb <- pnorm(lb,mu,sig)
  u <- runif(n,ulb,1)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 100000
set.seed(1234)
a_prior <- rbeta(n,1,6)
a_post <- rbeta(n,1,10)
b_prior <- rbeta(n,1,6)
b_post <- rbeta(n,1,10)
r_prior <- rbeta(n,1,6)
r_post <- rbeta(n,1,6)
s_prior <- rtruncnorm(n,25,10,0)
s_post <- rtruncnorm(n,25,10,0)
kappa_prior <- rtruncnorm(n,10,5,0)
kappa_post <- rtruncnorm(n,10,2,0)
delta_prior <- rtruncnorm(n,0,20,0)
kappaf_prior <- kappa_prior+delta_prior
kappaf_post <- rtruncnorm(n,40,15,15)

post_prior <- list(
  a = data.frame(prior=a_prior,post=a_post),
  b = data.frame(prior=b_prior,post=b_post),
  s = data.frame(prior=s_prior,post=s_post),
  r = data.frame(prior=r_prior,post=r_post),
  kappa = data.frame(prior=kappa_prior,post=kappa_post),
  kappaf = data.frame(prior=kappaf_prior,post=kappaf_post)
)
post_prior_plots <- list()
for(i in 1:length(parameters)){
  param_df_temp <- post_prior[[i]]
  post_prior_plots[[i]] <- post_plots[[i]]+
    geom_density(aes(x=prior,
                     linetype='prior'),
                 data=param_df_temp,
                 size=2)+
    geom_density(aes(x=post,
                     linetype='post'),
                 data=param_df_temp,
                 size=2)+
    theme(axis.text=element_text(size=13),
          axis.title=element_text(size=16),
          strip.text.x = element_text(size = 14),
          legend.position = 'none')
}
ggarrange(plotlist=post_prior_plots,
          nrow=2,ncol=3)
setwd(dir)
ggsave('./VWM/output/fig/fit_prev/post_prior_2.svg',
       height = 4,width = 7)
