source('./VWM/src/requires.R')
rm(list = ls())
Sys.setenv(STAN_NUM_THREADS = 4)

# s range from 50~1000
# grid: 50, 100, 200, 500, 800
# Fit Bays ---------

bays_data <- readRDS('./VWM/data/processed/bays_data.rds')

pw <- './VWM/output/results/fit_prev/bays'
if(!dir.exists(pw)){
  dir.create(pw)
}

parameters <- c('a','b','r','s',
                'kappa','kappaf',
                'xpred')
for(i in bays_data$subjID){
  ind_i <- bays_data$subject==i
  data_i <- list(
    nTrial = sum(ind_i),
    N = bays_data$N, M = bays_data$M,
    Setsize = bays_data$Setsize[ind_i],
    ind_mat = bays_data$ind_mat[ind_i,], 
    D = bays_data$Dist[ind_i,], 
    E = bays_data$E[ind_i,,], 
    x = bays_data$x[ind_i]
  )
  
  fit_im <- stan(file='./VWM/src/3_fit_prev/fit_im_1.stan',
                 data=data_i,
                 pars=parameters,
                 chains=4, 
                 cores=4,
                 seed = 123)
  saveRDS(fit_im,
          paste0(pw,'/subj_',i,'.rds'))
  rm(list = c('ind_i','data_i','fit_im'))
  Sys.sleep(1)
}

## pooled all individuals -----------
rm(list = ls())

bays_data <- readRDS('./VWM/data/processed/bays_data.rds')

pw <- './VWM/output/results/fit_prev/bays'
pw2 <- './VWM/output/fig/fit_prev/bays'
if(!dir.exists(pw2)) dir.create(pw2)

parameters <- c('a','b','r','s',
                'kappa','kappaf',
                'xpred')
m <- Setsize <- D <- response <- ypred_pool <- NULL
for(i in bays_data$subjID){
  ind_i <- bays_data$subjects==i
  m <- rbind(m, bays_data$m[ind_i,])
  Setsize <- c(Setsize,bays_data$Setsize[ind_i])
  D <- rbind(D, bays_data$Dist[ind_i,])
  response <- c(response, bays_data$response[ind_i])
  
  samples <- readRDS(paste0(pw,"/subj_",i,".rds"))
  
  traceplot(samples,pars=parameters[1:6])
  ggsave(paste0(pw2,"/subj_",i,"_trace.jpg"))
  jpeg(paste0(pw2,"/subj_",i,"_pairs.jpg"))
  pairs(samples,pars=parameters[1:6])
  dev.off()
  
  ypred <- t(rstan::extract(samples)$xpred)
  ypred_rad <- apply(ypred,2,function(u) bays_data$X[u])
  ypred_pool <- rbind(ypred_pool,ypred_rad)
}
fit_pool <- list(m=m,D=D,
                 Setsize=Setsize,
                 response=response,
                 ypred=ypred_pool)
saveRDS(fit_pool,
        './VWM/output/results/fit_prev/pool_results_bays.rds')

## plots ---------------
rm(list = ls())
bays_data <- readRDS('./VWM/data/processed/bays_data.rds')

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

pw <- './VWM/output/results/fit_prev'
pw2 <- './VWM/output/fig/fit_prev/bays'
fit_pool <- readRDS(paste0(pw,'/pool_results_bays.rds'))

source('./VWM/src/3_fit_prev/post_plots.R')

# Fit vdBerg ---------
rm(list=ls())
vdBerg_data <- readRDS('./VWM/data/processed/vdBerg_data.rds')

pw <- './VWM/output/results/fit_prev/vdBerg'
if(!dir.exists(pw)){
  dir.create(pw)
}

parameters <- c('a','b','r','s',
                'kappa','kappaf',
                'xpred')
for(i in vdBerg_data$subjID){
  ind_i <- vdBerg_data$subject==i
  data_i <- list(
    nTrial = sum(ind_i),
    N = vdBerg_data$N, M = vdBerg_data$M,
    Setsize = vdBerg_data$Setsize[ind_i],
    ind_mat = vdBerg_data$ind_mat[ind_i,], 
    D = vdBerg_data$Dist[ind_i,], 
    E = vdBerg_data$E[ind_i,,], 
    x = vdBerg_data$x[ind_i]
  )
  
  fit_im <- stan(file='./VWM/src/3_fit_prev/fit_im_2.stan',
                 data=data_i,
                 pars=parameters,
                 chains=4, 
                 cores=4,
                 seed = 123)
  saveRDS(fit_im,
          paste0(pw,'/subj_',i,'.rds'))
  rm(list = c('ind_i','data_i','fit_im'))
  Sys.sleep(1)
}

## pooled all individuals -----------
rm(list = ls())

vdBerg_data <- readRDS('./VWM/data/processed/vdBerg_data.rds')

pw <- './VWM/output/results/fit_prev/vdBerg'
pw2 <- './VWM/output/fig/fit_prev/vdBerg'
if(!dir.exists(pw2)) dir.create(pw2)

parameters <- c('a','b','r','s',
                'kappa','kappaf',
                'xpred')
m <- Setsize <- D <- response <- ypred_pool <- NULL
for(i in vdBerg_data$subjID){
  ind_i <- vdBerg_data$subjects==i
  m <- rbind(m, vdBerg_data$m[ind_i,])
  Setsize <- c(Setsize,vdBerg_data$Setsize[ind_i])
  D <- rbind(D, vdBerg_data$Dist[ind_i,])
  response <- c(response, vdBerg_data$response[ind_i])
  
  samples <- readRDS(paste0(pw,"/subj_",i,".rds"))
  
  traceplot(samples,pars=parameters[1:6])
  ggsave(paste0(pw2,"/subj_",i,"_trace.jpg"))
  jpeg(paste0(pw2,"/subj_",i,"_pairs.jpg"))
  pairs(samples,pars=parameters[1:6])
  dev.off()
  ypred <- t(rstan::extract(samples)$xpred)
  ypred_rad <- apply(ypred,2,function(u) vdBerg_data$X[u])
  ypred_pool <- rbind(ypred_pool,ypred_rad)
}
fit_pool <- list(m=m,D=D,
                 Setsize=Setsize,
                 response=response,
                 ypred=ypred_pool)
saveRDS(fit_pool,
        './VWM/output/results/fit_prev/pool_results_vdBerg.rds')

## plots ---------------
rm(list = ls())
vdBerg_data <- readRDS('./VWM/data/processed/vdBerg_data.rds')

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

pw <- './VWM/output/results/fit_prev/vdBerg'
pw2 <- './VWM/output/fig/fit_prev/vdBerg'
fit_pool <- readRDS(paste0(pw,'/pool_results.rds'))

source('./VWM/src/3_fit_prev/post_plots.R')
