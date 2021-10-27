rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayestestR)

choice_set <- read_csv("./RIC/data/previous/Choice.csv")%>%
  dplyr::select(Exp,x1,p1,t1,x2,p2,t2,N,y)
## t2>=1, t1&t2<=72, x1,x2<=15000

# group ---------------
data <- list(
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = choice_set$N,
  y = choice_set$y)
parameters <- c('a','logh','i','logs',
                'ypred')
samples <- stan(file='./RIC/src/3_fit_prev/fit_HD_ind.stan',
                data=data,
                pars=parameters,
                iter = 2000,
                warmup = 1000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
saveRDS(samples,
        './RIC/output/results/fit_prev/HD_group.rds')

## post pred ===========
ypred <- extract(samples,pars='ypred')$ypred
dim(ypred)
hist(ypred[,2])
ypred <- as.data.frame(ypred)
hdi_ypred <- bayestestR::hdi(ypred,ci=0.99)
dim(hdi_ypred)
hdi_ypred[1:10,]
choice_set$y[1:10]

post_pred <- data.frame(y = choice_set$y,
                        CI_high = hdi_ypred$CI_high,
                        CI_low = hdi_ypred$CI_low)%>%
  add_column(x = 1:nrow(choice_set))

post_pred1 <- post_pred[301:400,]
ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title='Group',x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave('./RIC/Output/fig/fit_prev/HD_post_group.png',
       height = 6,width = 18)

## post inference =============
png('./RIC/Output/fig/fit_prev/HD_pairs_group.png')
pairs(samples,pars = parameters[1:4])
dev.off()
png('./RIC/Output/fig/fit_prev/HD_trace_group.png')
traceplot(samples,pars = parameters[1:4])
dev.off()

post_param <- as.data.frame(summary(samples)$summary[1:4,])%>%
  rownames_to_column()
post_param
write_csv(post_param,'./RIC/output/results/fit_prev/HD_param.csv')

# individ ----------------
Set_list <- unique(choice_set$Exp)
Set_list
i <- Set_list[10]
choice_temp <- choice_set%>%filter(Exp==i)
dim(choice_temp)
data <- list(
  nTrial = nrow(choice_temp),
  x1 = choice_temp$x1, x2 = choice_temp$x2,
  t1 = choice_temp$t1, t2 = choice_temp$t2,
  o1 = 1/choice_temp$p1-1,
  o2 = 1/choice_temp$p2-1,
  N = choice_temp$N,
  y = choice_temp$y)
parameters <- c('a','logh','i','logs',
                'ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_HD_ind.stan',
                data=data,
                pars=parameters,
                iter = 4000,
                warmup = 2000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15,
                               adapt_delta = 0.99))
saveRDS(samples,
        paste0('./RIC/output/results/data_prior/HD_',i,'.rds'))

## post inf ===============
png(paste0('./RIC/Output/fig/fit_prev/HD/pairs_',i,
           '.png'))
pairs(samples,pars = parameters[1:4])
dev.off()

png(paste0('./RIC/Output/fig/fit_prev/HD/trace_',i,
           '.png'))
traceplot(samples,pars = parameters[1:4])
dev.off()

## post pred =========
ypred <- extract(samples,pars='ypred')$ypred
ypred <- as.data.frame(ypred)
dim(ypred)
hdi_ypred <- bayestestR::hdi(ypred)
hist(ypred[,1])
data$y[1]
post_pred <- data.frame(y = data$y,
                        CI_high = hdi_ypred$CI_high,
                        CI_low = hdi_ypred$CI_low)%>%
  arrange(y)%>%
  add_column(x = 1:length(data$y))

#post_i <- post_pred%>%filter(Exp_ind==1)
ggplot(post_pred,aes(x,y))+
  geom_point()+
  geom_segment(aes(xend=x,y=CI_low,yend=CI_high))+
  labs(title=i,x='Trial',y='# Option 1')+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0('./RIC/Output/fig/fit_prev/HD/post_',i,'.png'),
       height = 6,width = 6)

# hier ----------------
Set_list <- unique(choice_set$Exp)
Set_list
choice_set$Exp_ind <- rep(0,length(choice_set$Exp))
for(i in 1:length(Set_list)){
  choice_set$Exp_ind[choice_set$Exp==Set_list[i]] <- i
}

data<-list(
  nExp = length(Set_list),
  Exp = choice_set$Exp_ind,
  nTrial = nrow(choice_set),
  x1 = choice_set$x1, x2 = choice_set$x2,
  t1 = choice_set$t1, t2 = choice_set$t2,
  o1 = 1/choice_set$p1-1,
  o2 = 1/choice_set$p2-1,
  N = choice_set$N,
  y = choice_set$y)

parameters <- c('a','logh','i','s',
                'a_i','logh_i','i_i','s_i',
                'sd_i','ypred')
samples <- stan(file='./RIC/src/4_data_prior/fit_HD_hier.stan',
                data=data,
                pars=parameters,
                iter = 6000,
                warmup = 3000,
                chains=4, 
                thin=4,
                cores=4,
                seed = 123,
                verbose = TRUE,
                refresh = 100,
                control = list(max_treedepth = 15))
pairs(samples,pars = parameters[1:4])
traceplot(samples,pars = parameters[1:4])
saveRDS(samples,
        './RIC/output/results/data_prior/HD_hier.rds')
