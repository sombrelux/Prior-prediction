rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# fit --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))
post_param <- read_csv('./RIC/output/results/fit_pilot/RITCH_postparam.csv')
mu_post <- signif(post_param$mean,2)
sig_post <- signif(post_param$sd,2) 
parameters <- 'ypred'

Ub_to_list <- c(0.05,0.1,0.5,1,2)
for(i in c(1,5,10,20)){
  for(Ub_to in Ub_to_list){
    data<-list(
      nPart = 100,
      nTrial=nrow(choice_set),
      xs = choice_set$xs, ps = choice_set$ps, ts = choice_set$ts,
      xd = choice_set$xd, pd = choice_set$pd, td = choice_set$td,
      xr = choice_set$xr, pr = choice_set$pr, tr = choice_set$tr,
      mu_beta_xt = mu_post[1], mu_beta_xp = mu_post[2], 
      mu_beta_xa = mu_post[3], mu_beta_xr = mu_post[4],
      mu_beta_pa = mu_post[5], mu_beta_pr = mu_post[6],
      mu_beta_ta = mu_post[7], mu_beta_tr = mu_post[8],
      sig_beta_xt = sig_post[1]*i, sig_beta_xp = sig_post[2]*i, 
      sig_beta_xa = sig_post[3]*i, sig_beta_xr = sig_post[4]*i,
      sig_beta_pa = sig_post[5]*i, sig_beta_pr = sig_post[6]*i,
      sig_beta_ta = sig_post[7]*i, sig_beta_tr = sig_post[8]*i,
      #sig_beta_to = sig_beta_to)
      Ub_to = Ub_to)
    samples <- stan(file='./RIC/src/3_core_pred_pilot/prior_RITCH_normal.stan',
                    data=data, pars=parameters,
                    iter = 20000, warmup = 0,
                    chains = 4, cores = 4,
                    thin = 4, algorithm="Fixed_param")
    saveRDS(samples,
            paste0('./RIC/output/results/core_pred_pilot/prior_RITCH_normalx_',
                   i,'_',Ub_to,'.rds')) 
  }
}

# ci -----------
rm(list=ls())
library(tidyverse)
library(bayestestR)

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(i in c(1,5,10,20)){
  for(Ub_to in c(0.05,0.1,0.5,1,2)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred_pilot/prior_RITCH_normalx_',#normal_',
                            i,'_',Ub_to,'.rds'))
  ypred <- rstan::extract(samples)$ypred
  prop.1.Option <- matrix(data = NA,nrow = 20000,ncol = 384)
  for(j in 1:20000)  prop.1.Option[j,] <- rowMeans(ypred[j,,])
  prop.1.Option <- as.data.frame(prop.1.Option)
  
  hdi_ritch <- hdi(prop.1.Option,ci=0.9999)
  hdi_ritch <- hdi_ritch%>%
    add_column(model='RITCH',
               mean = apply(prop.1.Option,2,mean),
               manipulation=choice_set$manipulation,
               choice=choice_set$choice,
               trial_num=choice_set$num,
               trial=choice_set$trial)%>%
    group_by(manipulation,choice)%>%
    arrange(mean,.by_group = T)
  write_csv(hdi_ritch,paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_nx_',i,'_',Ub_to,'.csv'))

  ## hdi of manipulation effect ================
  base_ind <- choice_set$manipulation=='Base'
  mag_ind <- choice_set$manipulation=='Mag'
  cert_ind <- choice_set$manipulation=='Cert'
  imm_ind <- choice_set$manipulation=='Imm'
  
  manip_eff <- data.frame(prop.1.Option[,mag_ind] - prop.1.Option[,base_ind])%>%
    bind_cols(data.frame(prop.1.Option[,cert_ind] - prop.1.Option[,base_ind]))%>%
    bind_cols(data.frame(prop.1.Option[,imm_ind] - prop.1.Option[,base_ind]))
  
  hdi_manip_eff <- hdi(manip_eff,ci=0.9999)
  hdi_eff_ritch <- hdi_manip_eff%>%as.data.frame()%>%
    add_column(model = 'RITCH',
               manipulation = c(choice_set$manipulation[mag_ind],
                                choice_set$manipulation[cert_ind],
                                choice_set$manipulation[imm_ind]),
               choice = c(choice_set$choice[mag_ind],choice_set$choice[cert_ind],
                          choice_set$choice[imm_ind]),
               trial_num = c(choice_set$num[mag_ind],choice_set$num[cert_ind],
                             choice_set$num[imm_ind]),
               trial = c(choice_set$trial[mag_ind],choice_set$trial[cert_ind],
                         choice_set$trial[imm_ind]))
  write_csv(hdi_eff_ritch,
            paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_nx_',i,'_',Ub_to,'.csv'))
  rm(list = c('samples','ypred','prop.1.Option'))
  }
}


# plot ---------
rm(list=ls())
hdi_RITCH <- NULL
i = 5
Ub_to_list <- c(0.05,0.1,0.5,1,2)
for(Ub_to in Ub_to_list){
  hdi_RITCH_i <- read_csv(paste0('./RIC/output/results/core_pred_pilot/hdi_RITCH_eff_nx_',i,'_',Ub_to,'.csv'))
  hdi_RITCH_i$Ub <- Ub_to
  hdi_RITCH <- rbind(hdi_RITCH,hdi_RITCH_i)
}
dim(hdi_RITCH)

ggplot(hdi_RITCH,
       mapping = aes(x = trial_num,
                     group=Ub)) + 
  geom_ribbon(aes(ymin = CI_low, 
                  ymax = CI_high,
                  col = as.factor(Ub),
                  fill= as.factor(Ub)), 
              alpha = 0.3) + 
  facet_grid(manipulation~choice)+
  labs(x = "Trial", y = "Prop.Option.1",
       title="RITCH")

