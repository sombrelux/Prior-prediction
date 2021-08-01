source('./VWM/src/requires.R')
rm(list = ls())
# Pooled prior distributions -----------------
Set <- 'bays'
dir <- getwd()
setwd(paste0("./VWM/output/results/fit_prev/",Set))
subj_files <- list.files()
posterior_dist <- 
  array(dim=c(4000,length(subj_files),7))
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[i])
  posterior <- as.matrix(samples)
  posterior_delta <- posterior[,6] -
    posterior[,5]
  posterior_dist[,i,] <- 
    cbind(posterior[,1:6], posterior_delta)
  
}
dim(posterior_dist)

parameters <- c('a','b','r','s',
                'kappa','kappaf','delta')
post_plots <- list()
for(i in 1:length(parameters)){
  post_i <- as.data.frame(posterior_dist[,,i])%>%
    pivot_longer(cols = everything(),
                 names_to = 'subj',
                 values_to = 'posterior')
  post_plots[[i]] <- 
    ggplot(post_i)+
    geom_density(aes(x=posterior,group=subj,
                     col=subj,fill=subj),
                 alpha=0.1)+
    scale_x_continuous(parameters[i])+
    scale_y_continuous('')+
    theme(axis.text=element_text(size=11),
          axis.title=element_text(size=13),
          legend.position = 'none')
}
ggarrange(plotlist=post_plots,
          nrow=2,ncol=4)+
  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"))
setwd(dir)
ggsave(paste0('./VWM/output/fig/fit_prev/post_prior_',
              Set,'.png'),
        height = 4,width = 7)

# post to prior -----------
rtruncnorm <- function(n,mu,sig,lb){
  ulb <- pnorm(lb,mu,sig)
  u <- runif(n,ulb,1)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 100000
set.seed(1234)
a_prior <- rbeta(n,1,1)
a_post <- rbeta(n,1,10)
b_prior <- rbeta(n,1,1)
b_post <- rbeta(n,1,10)
r_prior <- rbeta(n,1,3)
r_post <- rbeta(n,1,6)
#s_prior <- runif(n,0,50)#rtruncnorm(n,25,10,0)
#s_post <- runif(n,0,50)#rtruncnorm(n,25,10,0)
kappa_prior <- rtruncnorm(n,10,5,0)
kappa_post <- rtruncnorm(n,5,5,5)
kappaf_prior <- rtruncnorm(n,25,5,0)
kappaf_post <- rtruncnorm(n,28,5,0)
delta_prior <- kappaf_prior-kappa_prior
delta_post <- kappaf_post-kappa_post

post_prior <- list(
  a = data.frame(prior=a_prior,post=a_post),
  b = data.frame(prior=b_prior,post=b_post),
  #s = data.frame(prior=s_prior,post=s_post),
  r = data.frame(prior=r_prior,post=r_post),
  kappa = data.frame(prior=kappa_prior,post=kappa_post),
  kappaf = data.frame(prior=kappaf_prior,post=kappaf_post),
  delta = data.frame(prior=delta_prior,post=delta_post)
)
post_prior_plots <- list()
for(i in 1:length(parameters)){
  param_df_temp <- post_prior[[i]]
  post_prior_plots[[i]] <- post_plots[[i]]+
    geom_density(aes(x=prior,
                     linetype='prior'),
                 data=param_df_temp,
                 size=2)+
    geom_density(aes(x=post,
                     linetype='post'),
                 data=param_df_temp,
                 size=2)+
    theme(axis.text=element_text(size=13),
          axis.title=element_text(size=16),
          strip.text.x = element_text(size = 14),
          legend.position = 'none')
}
ggarrange(plotlist=post_prior_plots,
          nrow=2,ncol=3)

#
