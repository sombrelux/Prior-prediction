#sample_prior <- sampling(model_prior, 
#                         data = data, chains = 4, thin = 1,
#                          warmup=1000, iter=2000,
#                          core=4, seed = 0)

#prior_samples <- rstan::extract(sample_prior)
ypred_prior<-prior_samples$ypred
saveRDS(ypred_prior,
        paste0(Output_dir,Type,'_',Model,"_ypred.rds"))

prop.1.Option<-data.frame(apply(ypred_prior,c(1,3),mean))
hdi_model<-hdi(prop.1.Option,ci=0.99)
hdi_model<-hdi_model%>%
  add_column(model=Model,
             m=apply(prop.1.Option,2,mean),
             manipulation=Type_set$manipulation,
             type=Type_set$choice,
             trial=Type_set$num)

saveRDS(hdi_model,
        paste0(Output_dir,Type,'_',Model,"_HDI.rds"))
