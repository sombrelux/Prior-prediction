# 1 Prior predictions ------------------


# 2 Plots ----------------------
ypred <- t(extract(samples)$ypred)
dim(ypred) #800 4000
range(ypred)
m <- data$m
D <- data$D
setsize <- data$Setsize

ypred_rad <- ypred/180*pi

wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

ytarg <- m[,1]
error_prior <- apply(ypred_rad,2,
                       function(u) wrap(u-ytarg))
dim(error_prior)
range(error_prior) #-pi,pi

error_prior <- data.frame(error_prior,
                            setsize=setsize)

## mae ===============
mean_err <- abs(error_prior)%>%
  dplyr::group_by(setsize)%>%
  summarise_at(vars(X1:X4000),mean)%>%
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
ind <- sample(4000,100)
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
  ylim(c(0,1.75))+
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
diff_prior <- apply(ypred_rad[trial,ind],2,
                      function(u) wrap(u-m[trial,-1]))
length(diff_prior)

diff_prior <- lapply(diff_prior, 
                       function(u) data.frame(u,
                                              setsize=setsize[trial]))
diff_prior <- lapply( diff_prior, 
                        function(u) {
                          apply(u,1,function(v) mean(v[1:(v[8]-1)]))
                        })
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
