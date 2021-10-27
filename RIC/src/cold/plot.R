
Erev_df$EV

ggplot(Erev_df,aes(x=EV,y=theta))+
  geom_point()+
  xlim(c(-80,80))+
  ylab('Proportion to choose safer option')+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
