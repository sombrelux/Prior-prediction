packages = c("tidyverse", "ggplot2",
             "ggpubr",
             "bayesplot","intervals",
             "HDIinterval","boot")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
dir.create('./RIC/data')
dir.create('./RIC/data/raw')
dir.create('./RIC/data/previous')
dir.create('./RIC/data/processed')
dir.create('./RIC/output')
dir.create('./RIC/output/results')
dir.create('./RIC/output/results/fit_prev')
dir.create('./RIC/output/results/core_pred')
dir.create('./RIC/output/fig')
dir.create('./RIC/output/fig/fit_prev')
dir.create('./RIC/output/fig/core_pred')