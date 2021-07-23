packages = c("tidyverse", "R.matlab","abind",
             "extraDistr","grDevices",
             "MASS","gridExtra","ggpubr",
              "bayesplot",
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
