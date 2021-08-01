source('./VWM/src/requires.R')
rm(list = ls())

# Bays --------------
## Pooled prior distributions -----------------

dir <- getwd()
setwd("./VWM/output/results/fit_prev/bays")
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
          axis.title=element_text(size=13))
}
#ggarrange(plotlist=post_plots,
 #         nrow=2,ncol=4)+
  #theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"))

#@ post to prior -----------
rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 100000
set.seed(1234)
a_prior <- rtruncnorm(n,0,0.5,0,1)
a_post <- rtruncnorm(n,0,0.2,0,1)
b_prior <- rtruncnorm(n,0,0.5,0,1)
b_post <- rtruncnorm(n,0,0.2,0,1)
r_prior <- rbeta(n,1,2)
r_post <- rbeta(n,1,4)
s_prior <- rtruncnorm(n,100,30,0,5000)
s_post <- rtruncnorm(n,100,30,0,5000)
kappa_prior <- rtruncnorm(n,10,3,0,5000)
kappa_post <- rtruncnorm(n,9,3,0,5000)
kappaf_prior <- rtruncnorm(n,40,10,0,5000)
kappaf_post <- rtruncnorm(n,40,10,0,5000)
delta_prior <- kappaf_prior-kappa_prior
delta_post <- rtruncnorm(n,30,10,0,5000)

post_prior <- list(
  a = data.frame(prior=a_prior,post=a_post),
  b = data.frame(prior=b_prior,post=b_post),
  r = data.frame(prior=r_prior,post=r_post),
  s = data.frame(prior=s_prior,post=s_post),
  kappa = data.frame(prior=kappa_prior,post=kappa_post),
  kappaf = data.frame(prior=kappaf_prior,post=kappaf_post),
  delta = data.frame(prior=delta_prior,post=delta_post)
)
post_prior_plots <- list()
for(i in 1:length(parameters)){
  param_df_temp <- post_prior[[i]]
  post_prior_plots[[i]] <- post_plots[[i]]+
    geom_density(aes(x=prior,
                     linetype='Prior'),
                 key_glyph= draw_key_smooth,
                 data=param_df_temp,
                 size=1,alpha=0.1)+
    geom_density(aes(x=post,
                     linetype='Posterior'),
                 data=param_df_temp,
                 key_glyph= draw_key_smooth,
                 size=1,alpha=0.1)+
    theme(axis.text=element_text(size=13),
          axis.title=element_text(size=16),
          strip.text.x = element_text(size = 14),
          legend.key.width = unit(0.5, 'cm'),
          legend.title = element_blank())+
   guides(
      colour = FALSE,
      fill =  FALSE)
}
ggarrange(plotlist=post_prior_plots,
          nrow=2,ncol=4,
          common.legend = TRUE, legend="bottom")

setwd(dir)

ggsave(paste0('./VWM/output/fig/fit_prev/post_prior_bays.png'),
       height = 4,width = 10)

# vdBerg ----------------
## Pooled prior distributions -----------------
rm(list = ls())
dir <- getwd()
setwd("./VWM/output/results/fit_prev/vdBerg")
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
          axis.title=element_text(size=13))
}

#@ post to prior -----------
rtruncnorm <- function(n,mu,sig,lb,ub){
  ulb <- pnorm(lb,mu,sig)
  uub <- pnorm(ub,mu,sig)
  u <- runif(n,ulb,uub)
  x <- qnorm(u)*sig+mu
  return(x)
}
n <- 100000
set.seed(1234)
a_prior <- rtruncnorm(n,0,0.2,0,1)
a_post <- rtruncnorm(n,0,0.1,0,1)
b_prior <- rtruncnorm(n,0,0.2,0,1)
b_post <- rtruncnorm(n,0,0.2,0,1)
r_prior <- rbeta(n,1,4)
r_post <- rbeta(n,1,5)
s_prior <- rtruncnorm(n,100,30,0,5000)
s_post <- rtruncnorm(n,100,30,0,5000)
kappa_prior <- rtruncnorm(n,9,3,0,5000)
kappa_post <- rtruncnorm(n,7,2,0,5000)
delta_prior <- rtruncnorm(n,30,10,0,5000)
delta_post <- rtruncnorm(n,35,10,0,5000)
kappaf_prior <- kappa_prior+delta_prior
kappaf_post <- kappa_post+delta_post

post_prior <- list(
  a = data.frame(prior=a_prior,post=a_post),
  b = data.frame(prior=b_prior,post=b_post),
  r = data.frame(prior=r_prior,post=r_post),
  s = data.frame(prior=s_prior,post=s_post),
  kappa = data.frame(prior=kappa_prior,post=kappa_post),
  kappaf = data.frame(prior=kappaf_prior,post=kappaf_post),
  delta = data.frame(prior=delta_prior,post=delta_post)
)
post_prior_plots <- list()
for(i in 1:length(parameters)){
  param_df_temp <- post_prior[[i]]
  post_prior_plots[[i]] <- post_plots[[i]]+
    geom_density(aes(x=prior,
                     linetype='Prior'),
                 key_glyph= draw_key_smooth,
                 data=param_df_temp,
                 size=1,alpha=0.1)+
    geom_density(aes(x=post,
                     linetype='Posterior'),
                 data=param_df_temp,
                 key_glyph= draw_key_smooth,
                 size=1,alpha=0.1)+
    theme(axis.text=element_text(size=13),
          axis.title=element_text(size=16),
          strip.text.x = element_text(size = 14),
          legend.key.width = unit(0.5, 'cm'),
          legend.title = element_blank())+
    guides(
      colour = FALSE,
      fill =  FALSE)
}
ggarrange(plotlist=post_prior_plots,
          nrow=2,ncol=4,
          common.legend = TRUE, legend="bottom")

setwd(dir)
ggsave(paste0('./VWM/output/fig/fit_prev/post_prior_vdBerg.png'),
       height = 4,width = 10)

