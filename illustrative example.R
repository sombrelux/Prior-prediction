rm(list = ls())
library(tidyverse)
n <- 100
N <- 100000

prob_high <- 1/(1+exp(-1))
resp_high <- c(rbinom(n*N/2,1,prob_high),rep(1,n*N/2))
resp_high <- matrix(resp_high,nrow=n,ncol=N,byrow = T)
resp_vague <- matrix(nrow=n,ncol=N)

for(i in 1:n){
  a_vague <- runif(N,0,100)
  prob_vague <- 1/(1+exp(-a_vague))
  resp_vague[i,] <- sapply(prob_vague, function(u) rbinom(1,1,u))
}

prop_high <- colMeans(resp_high)
prop_vague <- colMeans(resp_vague)

illust_df <- data.frame(prior='informative',prop=prop_high)%>%
  rbind(.,data.frame(prior='vague',prop=prop_vague))
ggplot(illust_df,aes(x=prop,group=prior))+
  stat_density(aes(fill=prior),alpha=0.4,bw=0.01)+
  labs(x='a',y='Density')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position = 'none')
ggsave("illustrate.png",
       height=4,width=4)
