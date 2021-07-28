packages = c("tidyverse", 
             "ggpubr",
             "bayesplot",
             "bayestestR","boot")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library(rstan)
options(mc.cores = parallel::detectCores())

if(!dir.exists('./RIC/data/processed')){
  dir.create('./RIC/data/processed')
}
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
