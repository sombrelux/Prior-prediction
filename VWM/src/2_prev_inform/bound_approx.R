source('./VWM/src/requires.R')
rm(list=ls())

# Estimate the precision parameters ------------
dir <- getwd()
setwd('./VWM/data/previous/vdBerg_data')
subj_files <- list.files()

exp_ind_1 <- c(1,2,3,8)
err_1 <- list()
for(j in 1:length(exp_ind_1)){
  file_ind <- grep(paste0('E',exp_ind_1[j],'_'),
                   subj_files)
  err_j <- NULL
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    err_vec <- subj_dt[[1]]
    set_size <- subj_dt[[3]]
    err_j <- c(err_j,err_vec[set_size==1])
  }
  err_1[[j]] <- err_j
}
err_1 <- unlist(err_1)

setwd(dir)
saveRDS(err_1,
        './VWM/output/results/previous/vdBerg_err_1.rds')

## upper bound of kappa, lower bound of kappa_f ====================

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

set.seed(12345)
N <- 10000
err_vm1 <- rvonmises(N,mu=0,kappa=18)
err_vm1 <- wrap(as.vector(err_vm1))
kappa_up <- data.frame(
  dataset = 'Pooled',
  error = err_1) %>%
  bind_rows(data.frame(
    dataset = 'von Mises(0, 18)',
    error = err_vm1)
  )
ggplot(kappa_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="")+
  scale_x_continuous("Response error")+
  scale_y_continuous("Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14))
ggsave("./VWM/output/fig/previous/kappaf_lw.svg",
       width=9)

## lower bound of kappa & upper bound of kappa_f ===============

d_von_mises<-function(x,pc,kappaf,kappa){
  y<-as.circular(x)
  0.33*dvonmises(y,0,kappaf)+(1-0.33)*dvonmises(y,0,kappa)
}
fitdistr(err_1,d_von_mises,
         start = list(kappa=16,kappaf = 20),
         lower = c(0,18),
         upper = c(18,Inf))


set.seed(12345)
N <- 10000
pc <- rbernoulli(N,0.33)
y_vm1 <- rvonmises(N,mu=0,kappa=5.5)
y_vm1<-wrap(as.vector(y_vm1))
y_vm2 <- rvonmises(N,mu=0,kappa=59)
y_vm2<-wrap(as.vector(y_vm2))
y <- pc*y_vm2+(1-pc)*y_vm1

kappaf_up <- data.frame(
  dataset = 'Pooled',
  error = err_1) %>%
  bind_rows(data.frame(
    dataset = 'Mixture',
    error = y)
  )

ggplot(kappaf_up,aes(x=error,group=dataset))+
  geom_density(aes(col=dataset,
                   fill=dataset),
               alpha=0.3)+
  labs(fill="",col="")+
  scale_x_continuous("Response error")+
  scale_y_continuous("Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 14))
ggsave("./VWM/output/fig/previous/kappaf_up.svg",
       width=9)

# Estimate the distance sensitivity parameter -----
rm(list=ls())

## lower bound ==================
## s should sensitive enough so that the effect of
## distance is unobservable in Bays' & vdBerg's data
## Bays, 60cm
## vdBerg, ~60cm
view_dist <- 0.6
radius <- tan(4.5/180*pi)*view_dist
smallest_dist <- (2*pi)/8*radius #~3.5
s <- 50
exp(-s*smallest_dist) #~0.16

## if the effect of s is observable in the experiment
## s should be smaller than a threshold
view_dist <- 0.5
radius <- tan((5.1/2)/180*pi)*view_dist
smallest_dist <- (2*pi)/13*radius
s <- 800
exp(-s*smallest_dist) #~2e-04

## upper bound =================
## Rerko et al., 2014
view_dist <- 0.5
radius <- tan((5.1/2)/180*pi)*view_dist
smallest_dist <- 4*pi/9*radius #~3.1

s <- c(50,100,150,200,300)
exp(-s*smallest_dist) 
#~0.2,0.04,0.009,0.002,9e-5