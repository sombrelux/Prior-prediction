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
    }
  }
)

dir.create('./VWM/data/processed')
dir.create('./VWM/output/results')
dir.create('./VWM/output/fig')
dir.create('./VWM/output/results/prior_prediction')
dir.create('./VWM/output/results/prior_prediction/subj')
dir.create('./VWM/output/results/data_prior')
dir.create('./VWM/output/results/testing')
