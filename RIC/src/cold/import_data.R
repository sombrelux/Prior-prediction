choice_set<-
  read_csv("./Data/processed/choice_set.csv")
Type_set<-choice_set%>%
  dplyr::filter(choice%in%Type,
                manipulation%in%c('Base','Mag',
                                  'Imm','Cert'))

p1<-Type_set$Option.1.Probability
p2<-Type_set$Option.2.Probability
t1<-Type_set$Option.1.Delay
t2<-Type_set$Option.2.Delay
x1<-Type_set$Option.1.Amount
x2<-Type_set$Option.2.Amount