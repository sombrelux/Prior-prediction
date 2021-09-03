packages = c("tidyverse","ggplot2",
             "R.matlab","abind",
             "extraDistr","grDevices","grid",
             "MASS","gridExtra","ggpubr",
             "bayesplot","data.table",
             "HDIinterval","circular")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
dir.create('./VWM/data/previous')
dir.create('./VWM/data/processed')
dir.create('./VWM/output/results')
dir.create('./VWM/output/results/fit_previous')
dir.create('./VWM/output/results/prior_prediction')
dir.create('./VWM/output/results/data_prior')
dir.create('./VWM/output/results/observations')
dir.create('./VWM/output/results/fit_observations')
dir.create('./VWM/output/fig')
dir.create('./VWM/output/fig/testing')
