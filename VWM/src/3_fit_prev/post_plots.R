ypred <- t(rstan::extract(samples)$xpred)
range(ypred)
dim(ypred)
ypred_rad <- apply(ypred,2,function(u) bins[u])
dim(ypred_rad)

# plots ------------
ytrue <- data_i$response
m <- data_i$m
D <- data_i$D
setsize <- data_i$Setsize

post_error <- apply(ypred_rad,2,
                     function(u) wrap(u-ytrue))
dim(post_error)
hist(post_error,freq = F)

## mae ===============
ytarg <- m[,1]
error_prior <- apply(ypred_rad,2,
                     function(u) wrap(u-ytarg))
dim(error_prior)
error_prior <- data.frame(error_prior,
                          setsize=setsize)

mean_err <- abs(error_prior)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X4000),~mean(.,na.rm=T))%>%
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
ggsave(paste0(pw2,"/subj_",i,"_mae.png"),
       width = 4.75,height = 4.75)

## response error =============
resp_err <- error_prior%>%
  pivot_longer(!setsize,names_to = 'sim',
               values_to = 'error')

set.seed(1234)
ind <- sample(1000,200)
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
ggsave(paste0(pw2,"/subj_",i,"_resp_err.png"),
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
ggsave(paste0(pw2,"/subj_",i,"_resp_err_zoom.png"),
       height=4, width = 6)

## deviation from non-targ ==============
trial <- setsize>1
x <- ypred_rad[trial,ind]
y <- split(x, rep(1:ncol(x), each = nrow(x)))
dev_nt <- lapply(y,
                function(u) wrap(u-m[trial,-1]))
length(dev_nt)
dim(dev_nt[[1]])

diff_prior <- 
  lapply(dev_nt, 
         function(u) data.frame(u,
                                setsize=setsize[trial]))
diff_prior <- 
  lapply( diff_prior, 
          function(u) {
            apply(u,1,function(v) mean(v[1:(v[6]-1)]))})
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
ggsave(paste0(pw2,"/subj_",i,"_err_nt.png"),
       width = 6,height = 4)

## deviation vs dist =============
Dist <- round(data_i$D[trial,],2)
dist_uniq <- sort(unique(Dist[,2]))
dist_uniq

setsize_dist <- setsize[trial]
error_dist <- list(data.frame(),
                   data.frame(),
                   data.frame(),
                   data.frame())
setsize_list<-c(2,4,6)
for(i in 1:3){
  size_temp <- setsize_list[i]
  set_size_ind <- setsize_dist==size_temp
  dev_nt_temp <- 
    lapply(dev_nt,function(u) u[set_size_ind,1:(size_temp-1)])
  dist_temp <- as.matrix(Dist[set_size_ind,2:size_temp],
                         nrow=sum(set_size_ind))
  
  for(k in 1:length(dist_uniq)){
    item_ind <- dist_temp==dist_uniq[k]
    error_temp <- sapply(dev_nt_temp,
                         function(u) u[item_ind],
                         simplify = T)
    error_dist[[k]] <- rbind(error_dist[[k]],
                         data.frame(error_temp,
                                    setsize=size_temp,
                                    dist=dist_uniq[k]))
  }
}

error_dist <- rbindlist(error_dist)%>%
  pivot_longer(X1:X200,names_to='sim',
               values_to = 'error')%>%
  mutate(dist = factor(dist))

ggplot(error_dist,aes(x=error,group=sim))+
  geom_density()+
  facet_grid(dist~setsize)+
  labs(x='Deviation from the non-target item',
       y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave(paste0(pw2,"/subj_",i,"_dist.png"),
       width = 10,height = 6)
