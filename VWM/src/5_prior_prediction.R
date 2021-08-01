source('./VWM/src/requires.R')
rm(list=ls())

# 1 Prior predictions ------------------
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
pw <- "./VWM/output/results/Prior_prediction/subj"
if(!dir.exists(pw)) dir.create(pw)
parameters <- 'ypred'
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
  samples <- stan(
    file='./VWM/src/4_tuning_priors/post_prior_3.stan',
    data=data,pars=parameters,iter = 500,warmup = 0,
    seed = 123, algorithm="Fixed_param")
  saveRDS(samples,
          paste0(pw,"/subj_",i,".rds"))
  
  rm(list=c('ind','data','samples'))
  Sys.sleep(2)
}
saveRDS(Setsize,"./VWM/output/results/Prior_prediction/setsize.rds")
saveRDS(D,"./VWM/output/results/Prior_prediction/D.rds")
saveRDS(m,"./VWM/output/results/Prior_prediction/m.rds")

dir <- getwd()
setwd('./VWM/output/results/Prior_prediction/subj')
subj_files <- list.files()
ypred <- NULL
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[i])
  ypred_i <- t(extract(samples)$ypred) #800, 2000
  ypred <- rbind(ypred,ypred_i)
}
dim(ypred)
setwd(dir)
saveRDS(ypred,"./VWM/output/results/Prior_prediction/prior_pred_pool.rds")

# 2 Plots ----------------------
rm(list=ls())
m <- readRDS("./VWM/output/results/Prior_prediction/m.rds")
D <- readRDS("./VWM/output/results/Prior_prediction/D.rds")
setsize <- readRDS("./VWM/output/results/Prior_prediction/setsize.rds")
ypred <- readRDS('./VWM/output/results/Prior_prediction/prior_pred_pool.rds')
dim(ypred) #16000 2000
range(ypred)

ypred_rad <- ypred/180*pi

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

ytarg <- m[,1]
error_ypred <- apply(ypred_rad,2,
                     function(u) wrap(u-ytarg))
error_ypred <- as.data.frame(t(error_ypred))
dim(error_ypred) #2000 16000

# core prediction for response error ------------
error_hdi<- hdi(error_ypred,ci=0.99) 
dim(error_hdi) #16000 4

error_core <- apply(error_ypred, 1, function(u) {
  u_temp <- u
  u[u_temp<error_hdi$CI_low] <- NA
  u[u_temp>error_hdi$CI_high] <- NA
  u})
dim(error_core)

error_core <- data.frame(error_core,
                          setsize=setsize)

mean_err <- abs(error_core)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X2000),~mean(.,na.rm=T))%>%
  pivot_longer(!setsize,names_to='sim',
               values_to = 'mean')
saveRDS(mean_err,"./VWM/output/results/Prior_prediction/mean_err.rds")

resp_err <- error_core%>%
  pivot_longer(!setsize,names_to = 'sim',
               values_to = 'error')
saveRDS(resp_err,"./VWM/output/results/Prior_prediction/resp_err.rds")

exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
error_true <- data.frame(true = wrap(exp1_dt$response - exp1_dt$m[,1]),
                         setsize = exp1_dt$Setsize)
saveRDS(error_true,"./VWM/output/results/Prior_prediction/resp_err_true.rds")

mae_true <- abs(error_true)%>%
  dplyr::group_by(setsize)%>%
  summarise(mae=mean(true))
saveRDS(mae_true,"./VWM/output/results/Prior_prediction/mean_err_true.rds")

### fig3 left in OL, 2017 ==========
ggplot(mean_err,aes(x=setsize))+
  geom_line(aes(y=mean,group=sim),alpha=0.02)+
  #geom_line(aes(y=mae),data=mae_true)+
  labs(x='Set size',y='Mean error')+
  scale_x_continuous(breaks = 1:8)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
       
ggsave("./VWM/output/fig/Prior_prediction/mae.png",width = 4.75,height = 4.75)

### fig3 middle ======
set.seed(1234)
ind <- sample(4000,200)
sim_sel <- paste0('X',ind)
resp_err_temp <- resp_err %>%
  filter(sim %in% sim_sel)
ggplot(resp_err_temp)+
  geom_density(aes(x=error,group=sim),alpha=0.02)+
  geom_density(aes(x=true),data=error_true)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Response error',y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")

ggsave("./VWM/output/fig/Prior_prediction/resp_err.png",height=4, width = 6)

### fig3 right =======
trial <- setsize>1
dev_nt <- apply(y_core[trial,ind],2,
                function(u) wrap(u-m[trial,-1]))
length(dev_nt)

diff_prior <- 
  lapply(dev_nt, 
         function(u) data.frame(u,
                                setsize=setsize[trial]))
diff_prior <- 
  lapply( diff_prior, 
          function(u) {
            apply(u,1,function(v) mean(v[1:(v[8]-1)]))})
diff_prior <- abind(diff_prior,along = 2)
dim(diff_prior)

diff_prior <- data.frame(setsize=setsize[trial],
                         diff_prior)%>%
  pivot_longer(!setsize,names_to='sim',
               values_to='error')

dim(diff_prior)

dev_nt_true <- wrap(exp1_dt$response - exp1_dt$m[,-1])
dev_nt_true <- data.frame(dev_nt_true,
                          setsize=exp1_dt$Setsize)%>%
  filter(setsize>1)
apply()

ggplot(diff_prior,aes(x=error,group=sim))+
  geom_density(alpha=0.02)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Deviation from non-target items',
       y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave("./VWM/output/fig/Prior_prediction/err_nt.png",width = 6,height = 4)

### fig7  =============
Dist <- round(data$D[trial,],2)
dist_uniq <- sort(unique(Dist[,2]))
dist_uniq

setsize_dist <- setsize[trial]
error_dist1 <- error_dist2 <- error_dist3 <- NULL
for(i in 1:5){
  set_size_ind <- setsize_dist==(i+3)
  dev_nt_temp <- 
    lapply(dev_nt,function(u) u[set_size_ind,1:(i+2)])
  dist_temp <- as.matrix(Dist[set_size_ind,2:(i+3)],
                         nrow=sum(set_size_ind))
  
  item_ind <- dist_temp==dist_uniq[1]
  error_1_temp <- sapply(dev_nt_temp,
                         function(u) u[item_ind],
                         simplify = T)
  error_dist1 <- rbind(error_dist1,
                       data.frame(error_1_temp,setsize=i+3))
  
  item_ind <- dist_temp==dist_uniq[2]
  error_2_temp <- sapply(dev_nt_temp,
                         function(u) u[item_ind],
                         simplify = T)
  error_dist2 <- rbind(error_dist2,
                       data.frame(error_2_temp,setsize=i+3))
  
  item_ind <- dist_temp>dist_uniq[2]
  error_3_temp <- sapply(dev_nt_temp,
                         function(u) u[item_ind],
                         simplify = T)
  error_dist3 <- rbind(error_dist3,
                       data.frame(error_3_temp,setsize=i+3))
  
}

error_dist <- rbind(
  data.frame(error_dist1,dist='1/13'),
  data.frame(error_dist2,dist='2/13'),
  data.frame(error_dist3,dist='>2/13')
)%>%
  pivot_longer(X1:X100,names_to='sim',
               values_to = 'error')%>%
  mutate(dist = factor(dist,levels=c('1/13','2/13','>2/13')))

ggplot(error_dist,aes(x=error,group=sim))+
  geom_density()+
  facet_grid(dist~setsize)+
  labs(x='Deviation from the non-target item',
       y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))

ggsave("./VWM/output/fig/Prior_prediction/dist.png",width = 10,height = 6)