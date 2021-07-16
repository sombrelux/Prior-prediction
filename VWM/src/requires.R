packages = c("tidyverse", "R.matlab","abind",
             "extraDistr","grDevices",
             "MASS","gridExtra","ggpubr",
             "rstan", "bayesplot",
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

options(mc.cores = parallel::detectCores())