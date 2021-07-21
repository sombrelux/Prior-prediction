remove.packages(c("StanHeaders", "rstan"))
install.packages("remotes")
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/",
                                          getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/",
                                    getOption("repos")))
library(rstan)
stan_version()

usethis::edit_r_makevars()

#https://blog.martinez.fyi/post/multithreading-and-map-reduce-in-stan/
#https://www.r-bloggers.com/2019/08/speeding-up-bayesian-sampling-with-map_rect/