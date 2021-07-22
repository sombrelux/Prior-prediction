source('./VWM/src/requires.R')
rm(list=ls())

# response errors ------------
dir <- getwd()
setwd('./VWM/data/previous/vdBerg_data')
subj_files <- list.files()

exp_ind <- c(1,2,3,8)
err_pool <- list()
for(j in 1:length(exp_ind)){
  err_temp <- NULL
  file_ind <- grep(paste0('E',exp_ind[j],'_'),
                   subj_files)
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    err_temp <- cbind(err_temp,
                      rbind(subj_dt[[1]], subj_dt[[3]]))
  }
  err_pool[[j]] <- err_temp
}

setwd(dir)
saveRDS(err_pool,
        './VWM/output/results/previous/vdBerg_err_pool.rds')

## mean abs error ===========
err_pool <-
  readRDS('./VWM/output/results/previous/vdBerg_err_pool.rds')
mean_error <- data.frame()
exp_label <- c(
  'WM, 2004','ZL, 2008','Bays, 2009','vdBerg, 2012'
)
for(i in 1:4){
  err_temp <- t(err_pool[[i]])
  err_temp <- as.data.frame(err_temp)
  colnames(err_temp) <- c('error','setsize')
  mean_error<- err_temp%>%
    group_by(setsize)%>%
    summarise(mae = mean(abs(error)))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mean_error)
}

mean_error
ggplot(mean_error,aes(setsize,mae,group=E,col=E))+
    geom_line()+
    geom_point()+
  labs(col='Dataset')+
  scale_x_continuous(
    "Set size",labels = 1:8,breaks=1:8)+
  scale_y_continuous(
    "Mean absolute error",limits = c(0.16,1.15),
    breaks = c(0.2,0.4,0.6,0.8,1))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./VWM/output/fig/previous/vdBerg_mae.svg",
       width = 7, height = 4.75)

## hist error =============
err_dt <- NULL
for(i in 1:4){
  err_temp <- t(err_pool[[i]])
  err_temp <- data.frame(err_temp,exp_label[i])
  colnames(err_temp) <- c('error','setsize','E')
  err_dt <- err_dt%>%
    bind_rows(err_temp)
}
ggplot(err_dt,aes(x=error,group=E,col=E))+
  geom_density(aes(fill=E),
               alpha=0.3)+
  facet_wrap(~setsize,nrow=2)+
  labs(fill='Dataset',col='Dataset')+
  scale_x_continuous("Response error")+
  scale_y_continuous("Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./VWM/output/fig/previous/vdBerg_resp_err.svg",
       height=4, width = 10)

ggplot(err_dt,aes(x=error,group=E,col=E))+
  geom_density(aes(fill=E),
               alpha=0.3)+
  facet_wrap(~setsize,nrow=2)+
  labs(fill='Dataset',col='Dataset')+
  scale_x_continuous("Response error")+
  scale_y_continuous("Density",limits = c(0,0.25))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./VWM/output/fig/previous/vdBerg_resp_err_zoom.svg",
       height=4, width = 10)

#  difference between resp & non-targ ------------
dir <- getwd()
setwd('./VWM/data/previous/vdBerg_data')
subj_files <- list.files()

exp_ind <- c(1,2,3,8)
err_nt_pool <- list()
for(j in 1:length(exp_ind)){
  file_ind <- grep(paste0('E',exp_ind[j],'_'),
                   subj_files)
  err_nt_temp <- NULL
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    err_nt_i <- sapply(subj_dt[[2]],
                       function(u) mean(u[[1]]), 
                       simplify = T)
    set_size_ind <- subj_dt[[3]]>1
    err_nt_temp <- rbind(err_nt_temp,
      cbind(err_nt_i[set_size_ind],subj_dt[[3]][set_size_ind]))
  }
  err_nt_pool[[j]] <- err_nt_temp
}

setwd(dir)
saveRDS(err_nt_pool,
        './VWM/output/results/previous/vdBerg_err_nt_pool.rds')

err_nt_pool <- readRDS('./VWM/output/results/previous/vdBerg_err_nt_pool.rds')
err_nt_dt <- NULL
for(i in 1:4){
  err_nt_temp <- err_nt_pool[[i]]
  err_nt_temp <- data.frame(err_nt_temp,exp_label[i])
  colnames(err_nt_temp) <- c('error','setsize','E')
  err_nt_dt <- err_nt_dt%>%
    bind_rows(err_nt_temp)
}
dim(err_nt_dt)

ggplot(err_nt_dt,aes(x=error,group=E,col=E))+
  geom_density(aes(fill=E),
               alpha=0.3)+
  facet_wrap(~setsize,nrow=2)+
  labs(fill='Dataset',col='Dataset')+
  scale_x_continuous("Deviation from non-target items")+
  scale_y_continuous("Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14))
ggsave("./VWM/output/fig/previous/vdBerg_err_nt.svg",
       height=4, width = 10)

# deviation from non-targ vs. distance -----------
rm(list=ls())

wrap = function(angle) { 
  #transform (-2pi,2pi) to (-pi,pi)
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

df_bays <- readMat('./VWM/data/previous/bays_data.mat')

Locations <- cbind(df_bays$target.pos,
                   df_bays$nontarget.pos)
unique_loc<-unique(Locations[,1])
unique_loc
unique_loc/unique_loc[2]

Locations_code <- Locations/unique_loc[2]
locations_rad <- (2*pi)*Locations_code/8
locations_dist <- locations_rad - locations_rad[,1]
Dist <- round(abs(wrap(locations_dist)),3)
table(Dist)

Colors <- cbind(df_bays$target,
                df_bays$nontarget)
col_diff <- wrap(Colors-Colors[,1])
dim(col_diff)

err_nt <- apply(Colors[,-1],2, 
                function(u) wrap(df_bays$response-u))
dim(err_nt)

unique(df_bays$n.items)
diff_dist <- NULL
for(i in c(2,4,6)){
  set_size_ind <- df_bays$n.items==i
  err_nt_temp <- err_nt[set_size_ind,1:(i-1)]
  setsize_temp <- df_bays$n.items[set_size_ind]
  col_diff_temp <- col_diff[set_size_ind,2:i]
  dist_temp <- as.matrix(Dist[set_size_ind,2:i],
                    nrow=sum(set_size_ind))
  dist_uniq <- unique(dist_temp[,1])
  error_temp <- NULL
  for(d in dist_uniq){
    item_ind <- dist_temp==d
    col_diff_d <- col_diff_temp[item_ind]
    error_d <- data.frame(
      setsize=i,dist=d,
      col_diff = col_diff_d,
      error=err_nt_temp[item_ind])
    error_temp <- error_temp%>%
      bind_rows(error_d)
  }
  any(is.na(error_temp))
  colSums(is.na(error_temp))
  
  diff_dist <- diff_dist %>% 
    bind_rows(error_temp)
}
dim(diff_dist)
hist(diff_dist$setsize)

p1 <- ggplot(diff_dist,aes(group=dist))+
  geom_density(aes(x=col_diff,
                   group=setsize,
                   col=factor(setsize),
                   fill=factor(setsize)),
               alpha=0.3)+
  facet_wrap(~dist,nrow=1)+
  labs(fill='Set size',col='Set size')+
  scale_x_continuous("Color difference between the target \n and non-target item")+
  scale_y_continuous("Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="none")

p2 <- ggplot(diff_dist,aes(group=dist))+
  geom_density(aes(x=error,
                   group=setsize,
                   col=factor(setsize),
                   fill=factor(setsize)),
               alpha=0.3)+
  facet_wrap(~dist,nrow=1)+
  labs(fill='Set size',col='Set size')+
  scale_x_continuous("Deviation from the non-target item")+
  scale_y_continuous("Density")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size = 14),
        legend.position="bottom")
ggarrange(p1,p2,nrow = 2,
          labels = c('A','B'),
          heights = c(0.8,0.9))
ggsave("./VWM/output/fig/previous/bays_dist.svg",
       height=5, width = 8)
