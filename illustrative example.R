library(tidyverse)
n <- 100
N <- 100000

resp_real <- resp_vague <- matrix(nrow=n,ncol=N)
for(i in 1:n){
  a_real <- runif(N,0,1)
  a_vague <- runif(N,0,100)
  prob_real <- 1/(1+exp(-a_real))
  prob_vague <- 1/(1+exp(-a_vague))
  resp_real[i,] <- sapply(prob_real, function(u) rbinom(1,1,u))
  resp_vague[i,] <- sapply(prob_vague, function(u) rbinom(1,1,u))
}

prop_real <- colMeans(resp_real)
prop_vague <- colMeans(resp_vague)

illust_df <- data.frame(prior='informative',prop=prop_real)%>%
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
