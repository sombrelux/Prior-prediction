packages = c("tidyverse", "ggplot2",
             "ggpubr",
             "bayesplot",
             "HDIinterval","boot")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
dir.create('./RIC/data/previous')
dir.create('./RIC/data/processed')
dir.create('./RIC/output/results')
dir.create('./RIC/output/results/fit_indiff')
dir.create('./RIC/output/results/prior_prediction')
dir.create('./RIC/output/results/testing')
dir.create('./RIC/output/results/observations')
dir.create('./RIC/output/fig')
