rm(list = ls())
library(tidyverse)
library(ggpubr)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# Fit individual ---------
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
parameters <- c('a','b','r','s',
                'kappa','delta')
#for(i in 1:exp1_dt$nPart){
i = 1
  ind_i <- exp1_dt$ID==i
  data_i <- list(
      nTrial = sum(ind_i),
      N = exp1_dt$N, M = exp1_dt$M,
      Setsize = exp1_dt$Setsize[ind_i],
      ind_mat = exp1_dt$ind_mat[ind_i,], 
      D = exp1_dt$D[ind_i,], 
      E = exp1_dt$E[ind_i,,], 
      x = exp1_dt$x[ind_i]
  )
    
  fit_im <- stan(file='./VWM/src/fit_im_exp1.stan',
                   data=data_i,
                   pars=parameters,
                   iter=2000,
                   warmup=1000,
                   chains=4, 
                   cores=4,
                   seed = 123)
  saveRDS(fit_im,
            paste0('./VWM/output/results/fit_previous/subj_',i,'.rds'))
  rm(list = c('ind_i','data_i','fit_im'))
#}

# plot posterior ------------------
rm(list = ls())
dir <- getwd()
setwd("./VWM/output/results/fit_previous")
subj_files <- list.files()
posterior_dist <- 
  array(dim=c(2000,length(subj_files),6))
for(i in 1:length(subj_files)){
  samples <- readRDS(subj_files[i])
  posterior <- as.matrix(samples)
  posterior_dist[,i,] <- posterior[,1:6]
}
dim(posterior_dist)
setwd(dir)

parameters <- c('a','b','r','s')
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
          nrow=1,ncol=4)+
  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"))
ggsave('./VWM/output/fig/exp1.png',
       width = 16,height = 4)
