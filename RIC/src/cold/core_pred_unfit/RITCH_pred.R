rm(list=ls())
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())

# normal priors --------------
choice_set <- read_csv('./RIC/data/processed/choice_set.csv')%>%
  filter(choice!='Dom')%>%
  mutate(xd = x1-x2,td = t2-t1, pd = p1-p2)%>%
  mutate(xs = sign(xd),ts = sign(td),
         ps = sign(pd),
         xr = 2*xd/(x1+x2),
         tr = ifelse(td==0,0,2*td/(t1+t2)),
         pr = 2*pd/(p1+p2))
post_param <- read_csv('./RIC/output/results/fit_prev/RITCH_param_choice.csv')
mu_post <- signif(post_param$mean,2)
sig_df <- read.csv('./RIC/src/4_core_pred_unfit/RITCH_sig.csv',header = T)
parameters <- 'ypred'

#sig_beta_xo_list <- c(0.017,0.083,0.17,0.83, 1.66)
#for(i in 1:4){
 # for(sig_beta_xo in sig_beta_xo_list){
    data<-list(
      nPart = 100,
      nTrial=nrow(choice_set),
      xs = choice_set$xs, ps = choice_set$ps, ts = choice_set$ts,
      xd = choice_set$xd, pd = choice_set$pd, td = choice_set$td,
      xr = choice_set$xr, pr = choice_set$pr, tr = choice_set$tr)
    samples <- stan(file='./RIC/src/4_core_pred_unfit/prior_RITCH_normal.stan',
                    data=data, pars=parameters,
                    iter = 20000, warmup = 0,
                    chains = 4, cores = 4,
                    thin = 4, algorithm="Fixed_param")
    saveRDS(samples,
            paste0('./RIC/output/results/core_pred_unfit/prior_RITCH_normal_',i,'_',sig_beta_xo,'.rds')) 
  }
}

## hdi of response =============
rm(list=ls())
library(tidyverse)
library(bayestestR)
i=4

choice_set <- read_csv("./RIC/data/processed/choice_set.csv")%>%
  filter(choice!='Dom')

for(sig_beta_xo in c(0.017,0.083,0.17,0.83, 1.66)){
  samples <- readRDS(paste0('./RIC/output/results/core_pred_unfit/prior_RITCH_normal_',i,
                            '_',sig_beta_xo,'.rds'))
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
  
  write_csv(hdi_ritch,paste0('./RIC/output/results/core_pred_unfit/hdi_RITCH_normal_',i,
                             '_',sig_beta_xo,'.csv'))
  
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
            paste0('./RIC/output/results/core_pred_unfit/hdi_RITCH_eff_',i,
                   '_',sig_beta_xo,'.csv'))
  rm(list = c('samples','ypred','prop.1.Option'))
  
}
