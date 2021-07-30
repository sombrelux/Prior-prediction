source('./VWM/src/requires.R')
rm(list = ls())
Sys.setenv(STAN_NUM_THREADS = 4)

bays_data <- readRDS('./VWM/data/processed/bays_data.rds')

# Fit individual ---------
s <- 2
pw <- paste0('./VWM/output/results/fit_prev/s=',s)
if(!dir.exists(pw)){
  dir.create(pw)
}

parameters <- c('a','b','r',
                'kappa','kappaf',
                'ypred')
for(i in bays_data$subjID){
  ind_i <- bays_data$subject==i
	data_i <- list(
	  nTrial = sum(ind_i), s=s,
	  N = bays_data$N, M = bays_data$M,
	  Setsize = bays_data$Setsize[ind_i],
	  ind_mat = bays_data$ind_mat[ind_i,], 
	  D = bays_data$Dist[ind_i,], 
	  E = bays_data$E[ind_i,,], 
	  x = bays_data$x[ind_i]
	)

	fit_im <- stan(file='./VWM/src/3_fit_prev/fit_im.stan',
	               data=data_i,
	               pars=parameters,
	               chains=4, 
	               cores=4,
	               seed = 123)
	saveRDS(fit_im,
	        paste0(pw,'/subj_',i,'.rds'))
	rm(list = c('ind_i','data','fit_im'))
	Sys.sleep(1)
}

# posterior predictive check -----------
s = 2
pw <- paste0('./VWM/output/results/fit_prev/s=',s)
pw2 <- paste0(paste0('./VWM/output/fig/fit_prev/s=',s))
if(!dir.exists(pw2)) dir.create(pw2)

for(i in bays_data$subjID){
  ind_i <- bays_data$subject==i
  data_i <- list(
    nTrial = sum(ind_i), s=s,
    N = bays_data$N, M = bays_data$M,
    Setsize = bays_data$Setsize[ind_i],
    ind_mat = bays_data$ind_mat[ind_i,], 
    D = bays_data$Dist[ind_i,], 
    E = bays_data$E[ind_i,,], 
    x = bays_data$x[ind_i]
  )
  samples <- readRDS(paste0(pw,"/subj_",i,".rds"))
  source('./VWM/src/3_fit_prev/post_plots.R')
}
