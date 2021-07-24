for(i in subjID){
  ind_i <- df_bays$subject==i
  data <- list(
    nTrial = sum(ind_i),
    N = N, M = M, s=s,
    Setsize = Setsize[ind_i],
    ind_mat = ind_mat[ind_i,], 
    D = Dist[ind_i,], 
    E = E[ind_i,,], 
    x = x[ind_i]
  )
  samples <- readRDS(paste0(pw,"/s=",s,".rds"))
  source('./VWM/src/3_fit_prev/post_plots.R')
}
