packages = c("tidyverse","data.table",
             "HDIinterval","boot",
             "ggpubr")

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
dir.create('./VWM/output/results/fit_prev')
dir.create('./VWM/output/results/prior_pred')
dir.create('./VWM/output/results/data_prior')
dir.create('./VWM/output/results/testing')
dir.create('./VWM/output/fig')
dir.create('./VWM/output/fig/fit_prev')
dir.create('./VWM/output/fig/testing')
dir.create('./VWM/output/fig/results')