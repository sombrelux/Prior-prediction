functions{  
  real partial_sum(int[] x_slice, int start, int end, vector[] theta) {
    int len = end-start+1;
    int k;
    real ctg_temp = 0.0;
    
    for(j in 1:len){
      k = start+j-1;
      ctg_temp += categorical_lpmf(x_slice[j]|theta[k]);
    } 
    return ctg_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> M;
  int<lower=1> N;
  vector<lower=1,upper=M>[nTrial] Setsize;
  matrix<lower=0,upper=1>[nTrial,M] ind_mat; //indices of presented stimulus
  matrix<lower=0,upper=3.15>[nTrial,M] D; //distance of location feature, the first element is the location of the target
  matrix<lower=-pi(),upper=pi()>[M,N] E[nTrial]; //possible response errors in radian
  int<lower=1,upper=N> x[nTrial]; // responses in degree
}
parameters{
  real<lower=0,upper=1> a;
  real<lower=0,upper=1> b;
  real<lower=0,upper=1> r;
  real<lower=0> s;
  real<lower=0> kappa;
  real<lower=0> delta;
}
transformed parameters{ 
  real<lower=0> kappaf=kappa+delta;
  matrix[M, N] vm_pdf_temp;
  matrix[nTrial,N] Ab = rep_matrix(Setsize, N); //background noise
  matrix[nTrial,N] Aa; //activation of features independent from location
  matrix[nTrial,N] Ac; //activation associated with location feature
  matrix[nTrial,N] Af; //additional activation of attention
  matrix[nTrial,N] Ax; //total activation
  vector[N] theta[nTrial];
  
  for(j in 1:nTrial){
      vm_pdf_temp = exp(kappa*cos(E[j]))/modified_bessel_first_kind(0,kappa);
      Ac[j] = (ind_mat[j].*exp(-s*D[j]))*vm_pdf_temp;
      Aa[j] = ind_mat[j]*vm_pdf_temp;
      Af[j] = exp(kappaf*cos(E[j,1]))/modified_bessel_first_kind(0,kappaf);
      Ax[j] = Ac[j]+Af[j]/Setsize[j]+(a*Aa[j]+b*Ab[j])*(Setsize[j]+r-1)/Setsize[j];
      theta[j] = Ax[j]'/sum(Ax[j]);
  }
  
}
model{
  int grainsize = 1;
  a ~ beta(1,1);
  b ~ beta(1,1);
  r ~ beta(1,1);
  s ~ normal(12,2);
  kappa ~ normal(10,2);
  delta ~ normal(1,1);
  //likelihood
  target += reduce_sum(partial_sum, x , grainsize, theta);
}
