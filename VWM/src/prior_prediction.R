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
  samples <- stan(file='./VWM/src/prior_4_g.stan',
                  data=data,
                  pars=parameters,
                  iter=2000, 
                  chains=4, 
                  thin=1,
                  cores=4,
                  warmup = 1000,
                  seed = 123,
                  algorithm="Fixed_param")
  saveRDS(samples,paste0("./VWM/output/results/Prior_4/Prediction/subj_",i,".rds"))
  Sys.sleep(30)
}
saveRDS(Setsize,"./VWM/output/results/Prior_4/setsize.rds")
saveRDS(D,"./VWM/output/results/Prior_4/D.rds")
saveRDS(m,"./VWM/output/results/Prior_4/m.rds")

dir <- getwd()
setwd('./VWM/output/results/Prior_4/Prediction')
subj_files <- list.files()
prior_4 <- NULL
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[i])
  ypred <- t(extract(samples)$ypred) #800, 4000
  prior_4 <- rbind(prior_4,ypred)
}
dim(prior_4)
setwd(dir)
saveRDS(prior_4,"./VWM/output/results/Prior_4/prior_pred_pool.rds")

# 2 Plots ----------------------
rm(list=ls())
m <- readRDS("./VWM/output/results/Prior_4/m.rds")
D <- readRDS("./VWM/output/results/Prior_4/D.rds")
setsize <- readRDS("./VWM/output/results/Prior_4/setsize.rds")
ypred <- readRDS('./VWM/output/results/Prior_4/prior_pred_pool.rds')

dim(ypred) #15200 4000
range(ypred)

ypred_rad <- ypred/180*pi

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

ytarg <- m[,1]
error_prior_4 <- apply(ypred_rad,2,
                       function(u) wrap(u-ytarg))
dim(error_prior_4)
range(error_prior_4) #-pi,pi

error_prior_4 <- data.frame(error_prior_4,
                            setsize=setsize)

mean_err <- abs(error_prior_4)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X4000),mean)%>%
  pivot_longer(!setsize,names_to='sim',
               values_to = 'mean')
saveRDS(mean_err,"./VWM/output/results/Prior_4/mean_err.rds")

resp_err <- error_prior_4%>%
  pivot_longer(!setsize,names_to = 'sim',
               values_to = 'error')
saveRDS(resp_err,"./VWM/output/results/Prior_4/resp_err.rds")

### fig3 left in OL, 2017 ==========
ggplot(mean_err,aes(x=setsize,y=mean,group=sim))+
  geom_line(alpha=0.02)+
  labs(x='Set size',y='Mean error')+
  scale_x_continuous(breaks = 1:8)
ggsave("./VWM/output/fig/Prior_4/Mean absolute response error.svg")

### fig3 middle ======
set.seed(1234)
ind <- sample(4000,100)
sim_sel <- paste0('X',ind)
resp_err_temp <- resp_err %>%
  filter(sim %in% sim_sel)
p <- ggplot(resp_err_temp,
            aes(x=error,group=sim))+
  geom_density(alpha=0.02)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Response error',y='Density')
p
ggsave("./VWM/output/fig/Prior_4/response error.svg")

### fig3 right =======
## differences between resp & non-targ
trial <- setsize>1
diff_prior_4 <- apply(ypred_rad[trial,ind],2,
                      function(u) wrap(u-m[trial,-1]))
length(diff_prior_4)

diff_prior_4 <- lapply(diff_prior_4, 
                       function(u) data.frame(u,
                                              setsize=setsize[trial]))
diff_prior_4 <- lapply( diff_prior_4, 
                        function(u) {
                          apply(u,1,function(v) mean(v[1:(v[8]-1)]))
                        })
diff_prior_4 <- abind(diff_prior_4,along = 2)
dim(diff_prior_4)

diff_prior <- data.frame(setsize=setsize[trial],
                         diff_prior_4)%>%
  pivot_longer(!setsize,names_to='sim',
               values_to='error')

dim(diff_prior)

p3 <- ggplot(diff_prior,aes(x=error,group=sim))+
  geom_density(alpha=0.02)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Deviation from non-target',
       y='Density')
p3
ggsave("./VWM/output/fig/Prior_4/diff.svg")

### fig7  =============
s=4
trial <- setsize==s
dist_temp <- round(D[trial,2:s],2)
dist_uniq <- sort(unique(dist_temp[,1]))
m_temp <- m[trial,2:s]
resp_temp <- ypred_rad[trial,ind]
error_temp <- apply(resp_temp,2,
                    function(u) wrap(u-m_temp))
length(error_temp)
dim(error_temp[[1]])

error_df<-NULL
for(i in 1:6){
  dist_i <- dist_temp==dist_uniq[i]
  error_i <- sapply(error_temp,
                    function(u) u[dist_i],
                    simplify = T)
  error_i <- cbind(error_i,
                   rep(dist_uniq[i],sum(dist_i)))
  error_df <- rbind(error_df,error_i)
}
error_df <- as.data.frame(error_df)%>%
  `colnames<-`(c(1:100,'distance'))%>%
  pivot_longer(1:100,names_to='sim',
               values_to = 'error')
head(error_df)

ggplot(error_df,aes(x=error,group=sim))+
  geom_density(alpha=0.02)+
  xlim(c(-pi,pi))+
  facet_wrap(~distance,nrow = 2)+
  labs(x='Deviation from non-target',
       y='Density')

ggsave("./VWM/output/fig/Prior_4/err_dist.svg")

# prior ------
hdi_im<-hdi(as.data.frame(t(error_prior_4)),ci=0.8)
hdi_im<-hdi_im%>%
  add_column(
    setsize = data$Setsize,
    mean = rowMeans(error_prior_4))%>%
  arrange(setsize,mean)%>%
  add_column(trial = rep(1:100,8))

ggplot(as.data.frame(hdi_im), 
       mapping = aes(x = trial,group=setsize)) + 
  geom_segment(aes(xend=trial,y = CI_low, yend = CI_high), 
               alpha = 0.35) +
  geom_point(mapping = aes(x = trial, y = mean))+
  facet_wrap(~setsize)



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

