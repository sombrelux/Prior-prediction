rm(list = ls())
library(tidyverse)
library(ggpubr)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 4)

# Fit individual ---------
exp4_dt <- readRDS('./VWM/data/processed/OL_exp4.rds')
pw <- './VWM/output/results/fit_observation'
parameters <- c('a','b','r','sloc','scol','w',
                'kappa','delta','xpred')
for(i in 11:21){#exp4_dt$nPart){
  ind_i <- exp4_dt$ID==i
  data_i <- list(
    nTrial = sum(ind_i),
    N = exp4_dt$N,
    M = exp4_dt$Setsize,
    Condition = exp4_dt$Condition[ind_i],
    Dloc = exp4_dt$Dloc[ind_i,],
    Dcol = exp4_dt$Dcol[ind_i,],
    E = exp4_dt$E[ind_i,,], 
    x = exp4_dt$x[ind_i]
  )
  
  fit_im <- stan(file='./VWM/src/fit_im_exp4.stan',
                 data=data_i,
                 pars=parameters,
                 iter=1500,
                 warmup=1000,
                 chains=4, 
                 cores=4,
                 seed = 123)
  saveRDS(fit_im,
          paste0(pw,'/subj_',i,'.rds'))
  rm(list = c('ind_i','data_i','fit_im'))
}

# plot posterior ------------------
  rm(list = ls())
  dir <- getwd()
  setwd("./VWM/output/results/fit_previous")
  subj_files <- list.files()
  posterior_dist <- 
    array(dim=c(2000,length(subj_files),8))
  for(i in 1:length(subj_files)){
    fit_im <- readRDS(subj_files[i])
    posterior <- as.matrix(fit_im)
    posterior_dist[,i,] <- posterior[,1:8]
  }
  dim(posterior_dist)
  
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
  setwd(dir)
  ggsave('./VWM/output/fig/exp1.png',
         width = 16,height = 4)
  