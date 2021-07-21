source('./RIC/src/requires.R')
rm(list=ls())

# RvA -------
source('./RIC/src/model_function.R')
choice_set <- read_csv("./RIC/data/processed/choice_set.csv")
RvA_set <- choice_set%>%filter(choice == 'RvA')%>%
	dplyr::select(x1,p1,t1,x2,p2,t2)%>%



## Nilsson et al., 2011
alpha <- 0.8; gamma <- 0.5
theta_cpt <- apply(RvA_set,2,function(u) cpt_theta(u[1],u[2],u[3],u[4],alpha,gamma)


# DvA --------

# RvD ---------

# DvAR ---------

# RvAD ---------

# DRvA --------