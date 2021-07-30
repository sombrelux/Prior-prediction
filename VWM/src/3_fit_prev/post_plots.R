ypred <- t(rstan::extract(samples)$xpred)
range(ypred)
dim(ypred)
ypred_rad <- apply(ypred,2,function(u) bins[u])
dim(ypred_rad)

# plots ------------
m <- data_i$m
D <- data_i$D
setsize <- data_i$Setsize
ytrue <- data_i$response
ytarg <- m[,1]

## mae ===============
error_true <- data.frame(error=wrap(ytrue-ytarg),
                         setsize=setsize)

error_post <- apply(ypred_rad,2,
                     function(u) wrap(u-ytarg))
dim(error_post)

error_post <- data.frame(error_post,
                          setsize=setsize)

mean_err <- abs(error_post)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X2000),~mean(.,na.rm=T))%>%
  pivot_longer(!setsize,names_to='sim',
               values_to = 'mean')

mae_true <- abs(error_true)%>%
  dplyr::group_by(setsize)%>%
  summarise(mean=mean(error))

ggplot(mean_err,aes(x=setsize,y=mean))+
  geom_line(aes(group=sim,col='Model'),alpha=0.02)+
  geom_line(aes(col='Observed'),data = mae_true)+
  labs(x='Set size',y='Mean error')+
  scale_x_continuous(breaks = 1:8)+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_blank())
ggsave(paste0(pw2,"/subj_",i,"_mae.png"),
       width = 7,height = 4.75)

## response error =============
resp_err <- error_post%>%
  pivot_longer(!setsize,names_to = 'sim',
               values_to = 'error')

set.seed(1234)
ind <- sample(2000,200)
sim_sel <- paste0('X',ind)
resp_err_temp <- resp_err %>%
  filter(sim %in% sim_sel)
ggplot(resp_err_temp,
       aes(x=error))+
  geom_density(aes(col='Model',
                   group=sim),
               key_glyph=draw_key_smooth,
               alpha=0.02)+
  geom_density(aes(col='Observed'),
               key_glyph=draw_key_smooth,
               data=error_true)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Response error',y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_blank())
ggsave(paste0(pw2,"/subj_",i,"_resp_err.png"),
       height=4, width = 6)

## deviation from non-targ ==============
trial <- setsize>1
x <- ypred_rad[trial,ind]
y <- split(x, rep(1:ncol(x), each = nrow(x)))
dev_nt <- lapply(y,
                function(u) wrap(u-m[trial,-1]))
length(dev_nt)
dim(dev_nt[[1]])

diff_post <- 
  lapply(dev_nt, 
         function(u) data.frame(u,
                                setsize=setsize[trial]))
diff_post <- 
  lapply( diff_post, 
          function(u) {
            apply(u,1,function(v) mean(v[1:(v[6]-1)]))})
diff_post <- abind(diff_post,along = 2)
dim(diff_post)

diff_post <- data.frame(setsize=setsize[trial],
                         diff_post)%>%
  pivot_longer(!setsize,names_to='sim',
               values_to='error')

dim(diff_post)

dev_nt_true <- wrap(ytrue[trial]-m[trial,-1])
diff_post_true <- apply(data.frame(dev_nt_true,
                                    setsize=setsize[trial]),
                         1,function(v) mean(v[1:(v[6]-1)]))
diff_post_true <- data.frame(error=diff_post_true,
                              setsize=setsize[trial])

ggplot(diff_post,aes(x=error))+
  geom_density(aes(group=sim,
                   col='Model'),
               key_glyph=draw_key_smooth,
               alpha=0.02)+
  geom_density(aes(col='Observed'),
               key_glyph=draw_key_smooth,
               data=diff_post_true)+
  xlim(c(-pi,pi))+
  facet_wrap(~setsize,nrow = 2)+
  labs(x='Deviation from non-target items',
       y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_blank())
ggsave(paste0(pw2,"/subj_",i,"_err_nt.png"),
       width = 6,height = 4)

## deviation vs dist =============
Dist <- round(data_i$D[trial,],2)
dist_uniq <- sort(unique(Dist[,2]))
dist_uniq

setsize_dist <- setsize[trial]
setsize_list<-c(2,4,6)
error_dist <- error_dist_true <- list(data.frame(),
                   data.frame(),
                   data.frame(),
                   data.frame())
for(j in 1:3){
  size_temp <- setsize_list[j]
  set_size_ind <- setsize_dist==size_temp
  dev_nt_temp <- 
    lapply(dev_nt,
           function(u) u[set_size_ind,1:(size_temp-1)])
  dev_nt_true_temp <- 
    dev_nt_true[set_size_ind,1:(size_temp-1)]
  dist_temp <- as.matrix(Dist[set_size_ind,2:size_temp],
                         nrow=sum(set_size_ind))
  
  for(k in 1:length(dist_uniq)){
    item_ind <- dist_temp==dist_uniq[k]
    error_temp <- sapply(dev_nt_temp,
                         function(u) u[item_ind],
                         simplify = T)
    error_true_temp <- dev_nt_true_temp[item_ind]
    error_dist[[k]] <- rbind(error_dist[[k]],
                         data.frame(error_temp,
                                    setsize=size_temp,
                                    dist=dist_uniq[k]))
    error_dist_true[[k]] <- rbind(error_dist_true[[k]],
                                  data.frame(error=error_true_temp,
                                             setsize=size_temp,
                                             dist=dist_uniq[k]))
  }
}

error_dist <- rbindlist(error_dist)%>%
  pivot_longer(X1:X200,names_to='sim',
               values_to = 'error')%>%
  mutate(dist = factor(dist))

error_dist_true <- rbindlist(error_dist_true)

ggplot(error_dist,aes(x=error))+
  geom_density(aes(group=sim,
                   col='Model'),
               key_glyph=draw_key_smooth,
               alpha=0.02)+
  geom_density(aes(col='Observed'),
               alpha=0.02,
               key_glyph=draw_key_smooth,
               data=error_dist_true)+
  facet_grid(dist~setsize)+
  labs(x='Deviation from the non-target item',
       y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.title = element_blank())
ggsave(paste0(pw2,"/subj_",i,"_dist.png"),
       width = 10,height = 6)
