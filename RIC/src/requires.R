library(tidyverse)

library(rstan)
options(mc.cores = parallel::detectCores())

library(bayestestR)
library(bayesplot)
library(ggpubr)
library(boot)

if(!dir.exists('./RIC/output/results')){
  dir.create('./RIC/output/results')
}
if(!dir.exists('./RIC/output/results/fit_prev')){
  dir.create('./RIC/output/results/fit_prev')
}
if(!dir.exists('./RIC/output/results/tuning_priors')){
  dir.create('./RIC/output/results/tuning_priors')
}
if(!dir.exists('./RIC/output/results/prior_prediction')){
  dir.create('./RIC/output/results/prior_prediction')
}
if(!dir.exists('./RIC/output/fig')){
  dir.create('./RIC/output/fig')
}
if(!dir.exists('./RIC/output/fig/fit_prev')){
  dir.create('./RIC/output/fig/fit_prev')
}
if(!dir.exists('./RIC/output/fig/tuning_priors')){
  dir.create('./RIC/output/fig/tuning_priors')
}
if(!dir.exists('./RIC/output/fig/prior_prediction')){
  dir.create('./RIC/output/fig/prior_prediction')
}
