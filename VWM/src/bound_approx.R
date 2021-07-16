source('./VWM/src/requires.R')
rm(list=ls())

dir <- getwd()

# Estimate the precision parameters ------------
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

exp_ind_2 <- c(2,3,8)
err_6 <- list()
for(j in 1:length(exp_ind_2)){
  file_ind <- grep(paste0('E',exp_ind_2[j],'_'),
                   subj_files)
  err_j <- NULL
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    err_vec <- subj_dt[[1]]
    set_size <- subj_dt[[3]]
    err_j <- c(err_j,err_vec[set_size==6])
  }
  err_6[[j]] <- err_j
}

setwd(dir)
saveRDS(err_1,'./VWM/output/results/vdBerg_err_1.rds')
saveRDS(err_6,'./VWM/output/results/vdBerg_err_6.rds')

## upper bound ====================
svg('./VWM/output/fig/vdBerg_err_1.svg',
    width=14,height=14)
par(mfrow=c(2,2),mar=c(5,5,5,5))
for(j in 1:length(exp_ind_1)){
  hist(err_1[[j]],
       breaks = seq(-3.6,3.6,0.3),
       xlim = c(-2,2),ylim=c(0,1.5),
       freq = F,xlab = 'Response errors',
       cex.lab = 2, cex.axis = 2, cex.main = 3,
       main = paste0('Histogram of E',exp_ind[j]))
}
dev.off()

err_pool <- unlist(err_1)
set.seed(12345)
d_von_mises <- function(x,kappa,Pb){
  y <- as.circular(x)
  Pb*dunif(x,min=-pi,max=pi)+(1-Pb)*dvonmises(y,0,kappa)
}
fitdistr(err_pool,d_von_mises,
         start = list(Pb=0.01,kappa = 5),
         lower = c(0,0),
         upper = c(1,Inf))

N <- 10000
pb <- rbernoulli(N,0.04)
y_vm <- rvonmises(N,mu=0,kappa=13)
y_vm<-as.vector(y_vm)
y_vm[y_vm>pi]<-y_vm[y_vm>pi]-2*pi
y <- pb*runif(N,-pi,pi)+(1-pb)*y_vm

y_vm_edf <- density(y_vm)
y_edf <- density(y)

svg('./VWM/output/fig/vdBerg_precision_1.svg')
hist(err_pool,
     breaks = seq(-3.6,3.6,0.2),
     xlim = c(-2,2),
     freq = F,xlab = 'Response errors',
     main = ''
)
lines(y_edf,lwd=2)
lines(y_vm_edf,col = 'red', lty = 'dashed',
      lwd = 2)
abline(h = 1/(2*pi), col = 'blue', 
       lty = 'dotted',lwd = 2)
legend("topright",
       legend = c('Mixture','von Mises',
                             'Uniform'),
       col = c('black','red','blue'),
       lty = c('solid','dashed','dotted'))
dev.off()

## lower bound ==========================
svg('./VWM/output/fig/vdBerg_err_6.svg',
    width=22,height=7)
par(mfrow=c(1,3),mar=c(5,5,5,5))
for(j in 1:length(exp_ind_2)){
  hist(err_6[[j]],
       breaks = seq(-3.6,3.6,0.3),
       xlim = c(-pi,pi),ylim=c(0,0.7),
       freq = F,xlab = 'Response errors',
       cex.lab = 2, cex.axis = 2, cex.main = 3,
       main = paste0('Histogram of E',exp_ind[j]))
}
dev.off()

err_pool_6 <- unlist(err_6)
set.seed(12345)
fitdistr(err_pool_6,d_von_mises,
         start = list(Pb=0.01,kappa = 5),
         lower = c(0,0),
         upper = c(1,Inf))

N <- 10000
pb <- rbernoulli(N,0.5)
y_vm <- rvonmises(N,mu=0,kappa=6.2)
y_vm<-as.vector(y_vm)
y_vm[y_vm>pi]<-y_vm[y_vm>pi]-2*pi
y <- pb*runif(N,-pi,pi)+(1-pb)*y_vm

y_vm_edf <- density(y_vm)
y_edf <- density(y)

svg('./VWM/output/fig/vdBerg_precision_6.svg')
hist(err_pool_6,
     breaks = seq(-3.6,3.6,0.2),
     xlim = c(-pi,pi),ylim = c(0,1),
     freq = F,xlab = 'Response errors',
     main = ''
)
lines(y_edf,lwd=2)
lines(y_vm_edf,col = 'red', lty = 'dashed',
      lwd = 2)
abline(h = 1/(2*pi), col = 'blue', 
       lty = 'dotted',lwd = 2)
legend("topright",
       legend = c('Mixture','von Mises',
                  'Uniform'),
       col = c('black','red','blue'),
       lty = c('solid','dashed','dotted'))
dev.off()

# Estimate the distance sensitivity parameter -----
rm(list=ls())

## upper bound =================
smallest_dist <- (2*pi)/13
max_vm <- dvonmises(pi/180,0,15)
dist_coef <- function(s) exp(-s*smallest_dist)
dist_coef(20)

## lower bound ==================
wrap = function(angle) { 
  #transform (-2pi,2pi) to (-pi,pi)
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

df_bays <- readMat('./VWM/data/previous/bays_data.mat')
ind <- df_bays$n.items == 2

Locations <- cbind(df_bays$target.pos,
                   df_bays$nontarget.pos)[ind,1:2]
unique_loc<-unique(Locations[,1])
unique_loc
unique_loc/unique_loc[7]

Locations_code <- Locations/unique_loc[7]
locations_rad <- (2*pi)*Locations_code/8
locations_dist <- locations_rad[,2] - locations_rad[,1]
Dist <- round(abs(wrap(locations_dist)),3)
table(Dist)

Colors <- cbind(df_bays$target,
                df_bays$nontarget)[ind,1:2]
col_diff <- wrap(Colors[,2]-Colors[,1])

resp_error <- df_bays$error[ind]

df_bays_2 <- data.frame(dist = Dist,
                        col_diff = col_diff,
                        error = resp_error)

p1 <- ggplot(df_bays_2,aes(group=dist))+
  stat_bin(aes(x=col_diff,y=..density..),
                 binwidth = 0.3)+
  facet_wrap(~dist,nrow=1)+
  labs(x='Color difference', y= 'Density')+
  xlim(c(-3.5,3.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 
                           "cm"),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
p1 <- annotate_figure(p1,fig.lab = 'A',
                      fig.lab.size = 20)
p2 <- ggplot(df_bays_2,aes(group=dist))+
  stat_bin(aes(x=error,y=..density..),
           binwidth = 0.3)+
  facet_wrap(~dist,nrow=1)+
  labs(x='Response error', y= 'Density')+
  xlim(c(-3.5,3.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5),
                           "cm"),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
p2 <- annotate_figure(p2,
                      fig.lab = 'B',
                      fig.lab.size = 20)

svg('./VWM/output/fig/bays_sens.svg',
    height = 9,width = 14)
grid.arrange(p1, p2, nrow = 2)
dev.off()

smallest_dist <- (2*pi)/8
max_vm <- dvonmises(pi/90,0,15)
dist_coef <- function(s) exp(-s*smallest_dist)
dist_coef(5)
