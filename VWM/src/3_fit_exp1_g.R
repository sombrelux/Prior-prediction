rm(list = ls())
library(tidyverse)
library(ggpubr)
library(rstan)
options(mc.cores = parallel::detectCores())
Sys.setenv(STAN_NUM_THREADS = 6)

# Fit group ---------
exp1_dt <- readRDS('./VWM/data/processed/OL_exp1.rds')
parameters <- c('a','b','r','s',
                'kappa','delta')

data <- list(
  nTrial = exp1_dt$nTrial,
  N = exp1_dt$N, M = exp1_dt$M,
  Setsize = exp1_dt$Setsize,
  ind_mat = exp1_dt$ind_mat, 
  D = exp1_dt$D, 
  E = exp1_dt$E, 
  x = exp1_dt$x
)

fit_im <- stan(file='./VWM/src/fit_im_exp1.stan',
               data=data,
               pars=parameters,
               iter=2000,
               refresh = 50,
               warmup=1000,
               chains=4, 
               cores=4,
               seed = 123)
saveRDS(fit_im,
        './VWM/output/results/fit_prev/exp1.rds')

# plot posterior ------------------
rm(list = ls())
samples <- readRDS('./VWM/output/results/fit_prev/exp1.rds')
posterior <- as.data.frame(samples)
dim(posterior)

parameters <- colnames(posterior)[1:6]
post_plots <- list()

for(i in 1:length(parameters)){
  post_i <- as.data.frame(posterior[,i])
  colnames(post_i) <- 'posterior'
  post_plots[[i]] <- 
    ggplot(post_i)+
    geom_density(aes(x=posterior),
                 alpha=0.1)+
    scale_x_continuous(parameters[i])+
    scale_y_continuous('')+
    theme(axis.text=element_text(size=11),
          axis.title=element_text(size=13))
}

ggarrange(plotlist=post_plots,
          nrow=2,ncol=3)+
  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"))
ggsave('./VWM/output/fig/exp1.png',
       width = 12,height = 4)
