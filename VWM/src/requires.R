packages = c("tidyverse", "R.matlab","abind",
             "extraDistr","grDevices","grid",
             "MASS","gridExtra","ggpubr",
              "bayesplot","data.table",
             "bayestestR","circular")

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


if(!dir.exists('./VWM/data/processed')){
  dir.create('./VWM/data/processed')
}
if(!dir.exists('./VWM/output/results')){
  dir.create('./VWM/output/results')
}
if(!dir.exists('./VWM/output/results/previous')){
  dir.create('./VWM/output/results/previous')
}
if(!dir.exists('./VWM/output/results/fit_prev')){
  dir.create('./VWM/output/results/fit_prev')
}
if(!dir.exists('./VWM/output/results/tuning_priors')){
  dir.create('./VWM/output/results/tuning_priors')
}
if(!dir.exists('./VWM/output/results/prior_prediction')){
  dir.create('./VWM/output/results/prior_prediction')
}
if(!dir.exists('./VWM/output/fig')){
  dir.create('./VWM/output/fig')
}
if(!dir.exists('./VWM/output/fig/previous')){
  dir.create('./VWM/output/fig/previous')
}
if(!dir.exists('./VWM/output/fig/fit_prev')){
  dir.create('./VWM/output/fig/fit_prev')
}
if(!dir.exists('./VWM/output/fig/tuning_priors')){
  dir.create('./VWM/output/fig/tuning_priors')
}
if(!dir.exists('./VWM/output/fig/prior_prediction')){
  dir.create('./VWM/output/fig/prior_prediction')
}
