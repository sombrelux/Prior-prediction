source('./RIC/src/requires.R')
rm(list=ls())

i = 4

hdi_model <- readRDS(paste0('./RIC/output/results/prior_prediction/prior_',i,'/hdi_model.rds'))

hdi_model$model <- ifelse(hdi_model$model=='RITCH',
                          'RITCH','Data prior')
hdi_model_p <- hdi_model%>%
  group_by(model,choice,manipulation,trial_num,trial,true)%>%
  summarise(lw=min(CI_low),
            up=max(CI_high))

ggplot(hdi_model_p, 
       mapping = aes(x = trial_num,group=model)) + 
  geom_ribbon(aes(ymin = lw, ymax = up,
                  fill=model), 
              alpha = 0.35) + 
  geom_point(aes(y=true))+
  facet_grid(manipulation~choice)+
  #ylim(0,1)+
  labs(x = "Trial", 
       y = "Prop.Option.1",
       title = 'Prior prediction')+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave(paste0('./RIC/output/fig/prior_prediction/prior_pred_',i,'.svg'),
       height = 6, width = 9)
