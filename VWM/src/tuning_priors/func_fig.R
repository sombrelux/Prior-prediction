ypred <- t(extract(samples)$ypred)
dim(ypred) #800 2000
range(ypred)
ypred_rad <- ypred/180*pi

# core prediction for each trial ------------
ypred_rad <- as.data.frame(t(ypred_rad))
ypred_hdi<-hdi(ypred_rad,ci=0.99) 
y_core <- apply(ypred_rad, 1, function(u) {
  u_temp <- u
  u[u_temp<ypred_hdi$CI_low] <- NA
  u[u_temp>ypred_hdi$CI_high] <- NA
  u})
dim(y_core)

# plots ------------
m <- data$m
D <- data$D
setsize <- data$Setsize

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

ytarg <- m[,1]
error_prior <- apply(y_core,2,
                     function(u) wrap(u-ytarg))
dim(error_prior)

error_prior <- data.frame(error_prior,
                          setsize=setsize)

## mae ===============
mean_err <- abs(error_prior)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X2000),~mean(.,na.rm=T))%>%
  pivot_longer(!setsize,names_to='sim',
               values_to = 'mean')

ggplot(mean_err,aes(x=setsize,y=mean,group=sim))+
  geom_line(alpha=0.02)+
  labs(x='Set size',y='Mean error')+
  scale_x_continuous(breaks = 1:8)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0(pw2,"/s=",s,"_mae.svg"),
       width = 4.75,height = 4.75)

## response error =============
resp_err <- error_prior%>%
  pivot_longer(!setsize,names_to = 'sim',
               values_to = 'error')

set.seed(1234)
ind <- sample(2000,200)
sim_sel <- paste0('X',ind)
resp_err_temp <- resp_err %>%
  filter(sim %in% sim_sel)
ggplot(resp_err_temp,
       aes(x=error,group=sim))+
  geom_density(alpha=0.02)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Response error',y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0(pw2,"/s=",s,"_resp_err.svg"),
       height=4, width = 6)

ggplot(resp_err_temp,
       aes(x=error,group=sim))+
  geom_density(alpha=0.02)+
  xlim(c(-pi,pi))+
  ylim(c(0,0.25))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Response error',y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggsave(paste0(pw2,"/s=",s,"_resp_err_zoom.svg"),
       height=4, width = 6)

## deviation from non-targ ==============
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
ggsave(paste0(pw2,"/s=",s,"_err_nt.svg"),
       width = 6,height = 4)

## deviation vs dist =============
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
ggsave(paste0(pw2,"/s=",s,"_dist.svg"),
       width = 10,height = 6)
