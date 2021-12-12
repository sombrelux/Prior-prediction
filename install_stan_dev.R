remove.packages(c("StanHeaders", "rstan"))
install.packages("remotes")
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/",
                                          getOption("repos")))

## install libcurl4-openssl-dev & libv8-dev in cmd

install.packages("rstan", repos = c("https://mc-stan.org/r-packages/",
                                    getOption("repos")))
library(rstan)
stan_version()

install.packages("usethis")
usethis::edit_r_makevars()

#CXX14FLAGS += -fPIC
#CXX14FLAGS += -DSTAN_THREADS
#CXX14FLAGS += -pthread
#CXX14=$(BINPREF)g++ -O2 -march=native -mtune=native
#CXX14FLAGS=-O3 -march=native -mtune=native -fPIC -Wno-unused-variable -Wno-unused-function
#CXX14=g++
#CPPFLAGS=-DUSE_STANC3

#https://blog.martinez.fyi/post/multithreading-and-map-reduce-in-stan/
#https://www.r-bloggers.com/2019/08/speeding-up-bayesian-sampling-with-map_rect/