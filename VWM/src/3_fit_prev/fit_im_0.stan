functions{  
  real partial_sum(int[] x_slice, int start, int end, int M, int N, vector Setsize, matrix ind_mat, matrix D, matrix[] E, real a, real b, real kappa, real kappaf,real s, real r) {
    int len = end-start+1;
    int k;
    matrix[M, N] vm_pdf_temp;
    matrix[len,N] Ab = rep_matrix(Setsize[start:end], N); //background noise
    matrix[len,N] Aa; //activation of features independent from location
    matrix[len,N] Ac; //activation associated with location feature
    matrix[len,N] Af; //additional activation of attention
    matrix[len,N] Ax; //total activation
    matrix[len,N] denom; //normalization of Ax
    vector[N] theta[len];
    real ctg_temp = 0.0;
    
    for(j in 1:len){
      k = start+j-1;
      vm_pdf_temp = exp(kappa*cos(E[k]))/modified_bessel_first_kind(0,kappa);
      Ac[j] = (ind_mat[k].*exp(-s*D[k]))*vm_pdf_temp;
      Aa[j] = ind_mat[k]*vm_pdf_temp;
      Af[j] = exp(kappaf*cos(E[k,1]))/modified_bessel_first_kind(0,kappaf);
      Ax[j] = Ac[j]+Af[j]/Setsize[k]+(a*Aa[j]+b*Ab[j])*(Setsize[k]+r-1)/Setsize[k];
      theta[j] = Ax[j]'/sum(Ax[j]);
	  ctg_temp += categorical_lpmf(x_slice[j]|theta[j]);
	  } 
    return ctg_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> M;
  int<lower=1> N;
  real<lower=0> s;
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
  real<lower=0> kappa;
  real<lower=0> kappaf;
}
model{
  int grainsize = 1;
  a ~ beta(1,1);
  b ~ beta(1,1);
  r ~ beta(1,3);
  kappa ~ normal(10,5);
  kappaf ~ normal(20,5);
  
  //likelihood
  target += reduce_sum(partial_sum, x , grainsize, M, N, Setsize, ind_mat,D, E, a, b,kappa, kappaf,s,r);
}
