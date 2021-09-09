rm(list=ls())
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
pw <- "./VWM/output/results/prior_prediction/"
prior_pred_ind <- readRDS(paste0(pw,
                             "/prior_weak_pool/prior_pred.rds"))
prior_pred_hier <- readRDS(paste0(pw,
                          "/prior_narrow_pool/prior_pred.rds"))
prior_pred_pool <- readRDS(paste0(pw,
                          "/prior_inform_pool/prior_pred.rds"))
ytarg <- prior_pred_hier$Orient[,1]
y1h<- wrap(prior_pred_hier$ypred[5,]-ytarg[5])
y1i<- wrap(prior_pred_ind$ypred[5,]-ytarg[5])
y1p<- wrap(prior_pred_pool$ypred[5,]-ytarg[5])
y1<-rbind(data.frame(type='narrow',y=y1h),
      data.frame(type='weak',y=y1i),
      data.frame(type='inform',y=y1p))
dim(y1)
ggplot(y1,aes(x=y,group=type))+
  geom_density(aes(col=type))
