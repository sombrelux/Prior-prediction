# Pool posterior estimate of parameters
# take them as prior 1

source('./RIC/src/requires.R')
rm(list=ls())

# HD -------
post_hd <- 
  read_csv('./RIC/output/results/fit_prev/HD_Erev_stats.csv')[1:4,1:3]%>%
  add_column(set='Erev')%>%bind_rows(
  read_csv('./RIC/output/results/fit_prev/HD_Ericson_stats.csv')[1:4,1:3]%>%
  add_column(set='Ericson'))%>%bind_rows(
  read_csv('./RIC/output/results/fit_prev/HD_GW_stats.csv')[1:4,1:3]%>%
  add_column(set='GW'))%>%bind_rows(
  read_csv('./RIC/output/results/fit_prev/HD_SR_stats.csv')[1:4,1:3]%>%
  add_column(set='SR'))%>%
  arrange(X1)%>%
  rename(Parameter=X1)
write_csv(post_hd,'./RIC/output/results/fit_prev/HD_stats.csv')

# MHD -------
post_mhd <- 
  read_csv('./RIC/output/results/fit_prev/MHD_Erev_stats.csv')[1:7,1:3]%>%
  add_column(set='Erev')%>%bind_rows(
    read_csv('./RIC/output/results/fit_prev/MHD_Ericson_stats.csv')[1:7,1:3]%>%
      add_column(set='Ericson'))%>%bind_rows(
        read_csv('./RIC/output/results/fit_prev/MHD_GW_stats.csv')[1:7,1:3]%>%
          add_column(set='GW'))%>%bind_rows(
            read_csv('./RIC/output/results/fit_prev/MHD_SR_stats.csv')[1:7,1:3]%>%
              add_column(set='SR'))%>%
  arrange(X1)%>%
  rename(Parameter=X1)
write_csv(post_mhd,'./RIC/output/results/fit_prev/MHD_stats.csv')

# PTT -------
post_ptt <- 
  read_csv('./RIC/output/results/fit_prev/PTT_Erev_stats.csv')[1:5,1:3]%>%
  add_column(set='Erev')%>%bind_rows(
    read_csv('./RIC/output/results/fit_prev/PTT_Ericson_stats.csv')[1:5,1:3]%>%
      add_column(set='Ericson'))%>%bind_rows(
        read_csv('./RIC/output/results/fit_prev/PTT_GW_stats.csv')[1:5,1:3]%>%
          add_column(set='GW'))%>%bind_rows(
            read_csv('./RIC/output/results/fit_prev/PTT_SR_stats.csv')[1:5,1:3]%>%
              add_column(set='SR'))%>%
  arrange(X1)%>%
  rename(Parameter=X1)
write_csv(post_ptt,'./RIC/output/results/fit_prev/PTT_stats.csv')

# RITCH -------
post_RITCH <- 
  read_csv('./RIC/output/results/fit_prev/RITCH_Erev_stats.csv')[1:8,1:3]%>%
  add_column(set='Erev')%>%bind_rows(
    read_csv('./RIC/output/results/fit_prev/RITCH_Ericson_stats.csv')[1:8,1:3]%>%
      add_column(set='Ericson'))%>%bind_rows(
        read_csv('./RIC/output/results/fit_prev/RITCH_GW_stats.csv')[1:8,1:3]%>%
          add_column(set='GW'))%>%
  arrange(X1)%>%
  rename(Parameter=X1)
write_csv(post_RITCH,'./RIC/output/results/fit_prev/RITCH_stats.csv')
