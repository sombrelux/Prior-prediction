source('./VWM/src/requires.R')
rm(list=ls())

# 1 Prior predictions ------------------
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
parameters <- c('a','b','kappa',
                'kappaf','s','r',
                'ypred')
Setsize <- m <- D <- NULL
for(i in 1:exp1_dt$nPart){
  ind <- exp1_dt$ID==i
  data <- list(nTrial=sum(ind), 
               M=exp1_dt$M,N=exp1_dt$N,
               Setsize=exp1_dt$Setsize[ind],
               X=exp1_dt$X,
               D=exp1_dt$D[ind,],m=exp1_dt$m[ind,])
  Setsize <- c(Setsize, data$Setsize)
  D <- rbind(D, data$D)
  m <- rbind(m, data$m)
  samples <- stan(file='./VWM/src/prior_1_g.stan',
                  data=data,
                  pars=parameters,
                  iter=2000, 
                  chains=4, 
                  thin=1,
                  cores=4,
                  warmup = 1000,
                  seed = 123,
                  algorithm="Fixed_param")
  saveRDS(samples,paste0("./VWM/output/results/Prior_1/subj_",i,".rds"))
  Sys.sleep(30)
}
saveRDS(Setsize,"./VWM/output/results/Prior_1/setsize.rds")
saveRDS(D,"./VWM/output/results/Prior_1/D.rds")
saveRDS(m,"./VWM/output/results/Prior_1/m.rds")

dir <- getwd()
setwd('./VWM/output/results/Prior_1')
subj_files <- list.files()
prior_1 <- NULL
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[1])
  ypred <- t(extract(samples)$ypred) #800, 4000
  prior_1 <- rbind(prior_1,ypred)
}
dim(prior_1)
saveRDS(prior_1,"prior_pred_pool.rds")
setwd(dir)

# 2 Plots ----------------------
m <- readRDS("./VWM/output/results/Prior_1/m.rds")
setsize <- readRDS("./VWM/output/results/Prior_1/setsize.rds")
ypred <- readRDS('./VWM/output/results/Prior_1/prior_pred_pool.rds')

dim(ypred) #15200 4000
range(ypred)

ypred_rad <- ypred/180*pi

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

ytarg <- m[,1]
error_prior_1 <- apply(ypred_rad,2,
                       function(u) wrap(u-ytarg))
dim(error_prior_1)
range(error_prior_1) #-pi,pi

error_prior_1 <- data.frame(error_prior_1,
                            setsize=setsize)

mean_err <- abs(error_prior_1)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X4000),mean)%>%
  pivot_longer(!setsize,names_to='sim',
               values_to = 'mean')
saveRDS(mean_err,"./VWM/output/results/Prior_1/mean_err.rds")

resp_err <- error_prior_1%>%
  pivot_longer(!setsize,names_to = 'sim',
               values_to = 'error')
saveRDS(resp_err,"./VWM/output/results/Prior_1/resp_err.rds")

### fig3 left in OL, 2017 ==========

ggplot(mean_err,aes(x=setsize,y=mean,group=sim))+
  geom_line(alpha=0.02)+
  labs(x='Set size',y='Mean error')+
  scale_x_continuous(breaks = 1:8)
ggsave("./VWM/output/fig/Prior_1/Mean absolute response error.svg")

### fig3 middle ======
set.seed(1234)
sim_sel <- paste0('X',sample(4000,500))
resp_err_temp <- resp_err %>%
  filter(sim %in% sim_sel)
ggplot(resp_err,
       aes(x=error,group=sim))+
  geom_histogram(aes(y=..density..),
                 color = "orchid2",
                 binwidth = 0.3,
                 alpha=0.5)+
  facet_wrap(~setsize,nrow = 1)+
  labs(x='Response error',y='Density')



### fig3 right =======
## differences between resp & non-targ
diff_prior_1 <- apply(ypred_rad,1,
                      function(u) wrap(u-data$m[,-1]))
diff_prior_1 <- lapply(diff_prior_1, 
                       function(u) data.frame(u,
                                              setsize=data$Setsize))
diff_prior_1 <- lapply( diff_prior_1, 
                        function(u) {
                          apply(u,1,function(v) mean(v[1:v[8]]))
                        })
diff_prior_1 <- abind(diff_prior_1,along = 2)
dim(diff_prior_1)

diff_prior <- data.frame(setsize=data$Setsize,
                         diff_prior_1)%>%
  pivot_longer(!setsize,names_to='sim',
               values_to='error')

dim(diff_prior)

p3 <- ggplot(diff_prior,aes(x=error,group=sim))+
  geom_density(alpha=0.6)+
  facet_wrap(~setsize)+
  labs(x='Difference from non-target')

svg('./VWM/output/fig/prior_1.svg',
    height = 9,width = 21)
grid.arrange(p1, p2, p3, nrow = 1)
dev.off()

# prior ------
hdi_im<-hdi(as.data.frame(t(error_prior_1)),ci=0.8)
hdi_im<-hdi_im%>%
  add_column(
    setsize = data$Setsize,
    mean = rowMeans(error_prior_1))%>%
  arrange(setsize,mean)%>%
  add_column(trial = rep(1:100,8))

ggplot(as.data.frame(hdi_im), 
       mapping = aes(x = trial,group=setsize)) + 
  geom_segment(aes(xend=trial,y = CI_low, yend = CI_high), 
               alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = mean))+
  facet_wrap(~setsize)