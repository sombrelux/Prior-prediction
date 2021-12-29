
# vdBerg orientations ----------------
library(R.matlab)
dir <- getwd()
setwd('./VWM/data/previous')
subj_files <- list.files()

exp_ind <- c(2,3,7,8,9,10)

err_pool <- err_nt_pool <- list()
for(j in 1:length(exp_ind)){
  err_temp <- err_nt_temp <- NULL
  file_ind <- grep(paste0('E',exp_ind[j],'_'),
                   subj_files)
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    setsize_ind <- subj_dt[[3]]==6
    err_temp <- rbind(err_temp,
                      data.frame(id=i,
                                 err=subj_dt[[1]][setsize_ind]))
    err_nt_i <- sapply(subj_dt[[2]],
                       function(u) mean(abs(u[[1]])), 
                       simplify = T)
    err_nt_temp <- rbind(err_nt_temp,
                         data.frame(id=i,
                                    err=err_nt_i[setsize_ind]))
  }
  
  err_pool[[j]] <- err_temp
  err_nt_pool[[j]] <- err_nt_temp
}

setwd(dir)
saveRDS(err_pool,
        './VWM/data/processed/vdBerg_resp_err.rds')

saveRDS(err_nt_pool,
        './VWM/data/processed/vdBerg_dev_nt.rds')

## resp err ===========
err_pool <-
  readRDS('./VWM/data/processed/vdBerg_resp_err.rds')
mae_resp_err <- data.frame()
exp_label <- c(
  '2','3','6','7','vdBerg, 2012','RTT, 2012'
)
for(i in 1:6){
  err_temp <- err_pool[[i]]
  mae_resp_err <- err_temp%>%
    group_by(id)%>%
    summarise(mae = mean(abs(err)))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mae_resp_err)
}

mae_resp_err
write_csv(mae_resp_err,
          './VWM/output/results/data_prior/resp_err_dp.csv')

## dev_nt =============
dev_nt_pool <- 
  readRDS('./VWM/data/processed/vdBerg_dev_nt.rds')

mae_dev_nt <- data.frame()
for(i in 1:6){
  dev_nt_temp <- dev_nt_pool[[i]]
  mae_dev_nt<- dev_nt_temp%>%
    group_by(id)%>%
    summarise(mae = mean(err))%>%
    add_column(E=exp_label[i]) %>%
    bind_rows(mae_dev_nt)
}

mae_dev_nt
write_csv(mae_dev_nt,
          './VWM/output/results/data_prior/dev_nt_dp.csv')

# vdBerg orientations ==========
dir <- getwd()
setwd('./VWM/data/previous')
subj_files <- list.files()

exp_ind <- c(9,10)
err_pool <- err_nt_pool <- list()
for(j in 1:length(exp_ind)){
  err_temp <- err_nt_temp <- NULL
  file_ind <- grep(paste0('E',exp_ind[j],'_'),
                   subj_files)
  for(i in file_ind){
    subj_dt <- readMat(subj_files[i])$data
    setsize_ind <- subj_dt[[3]]==6
    err_temp <- rbind(err_temp,
                      data.frame(id=i,
                                 err=subj_dt[[1]][setsize_ind]))
    err_nt_i <- sapply(subj_dt[[2]],
                       function(u) mean(abs(u[[1]])), 
                       simplify = T)
    err_nt_temp <- rbind(err_nt_temp,
                         data.frame(id=i,
                                    err=err_nt_i[setsize_ind]))
  }
  
  err_pool[[j]] <- err_temp
  err_nt_pool[[j]] <- err_nt_temp
}

# theoretical lower limit -----------------
x1<-runif(10000,0,1)
hist(x1)
x2<-runif(10000,0,1)
hist(x2)
y<-x2-x1
hist(y)