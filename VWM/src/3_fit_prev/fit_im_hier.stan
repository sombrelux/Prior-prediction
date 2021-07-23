functions{  
  real partial_sum(int[] x_slice, int start, int end, int[] ID, int M, int N, vector Setsize, matrix ind_mat, matrix D, matrix[] E, vector a, vector b, vector r, vector kappa, vector kappaf, real s) {
    int len = end-start+1;
    int k;
    int id;
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
      id = ID[k];
      vm_pdf_temp = exp(kappa[id]*cos(E[k]))/modified_bessel_first_kind(0,kappa[id]);
      Ac[j] = (ind_mat[k].*exp(-s*D[k]))*vm_pdf_temp;
      Aa[j] = ind_mat[k]*vm_pdf_temp;
      Af[j] = exp(kappaf[id]*cos(E[k,1]))/modified_bessel_first_kind(0,kappaf[id]);
      Ax[j] = Ac[j]+Af[j]/Setsize[k]+(a[id]*Aa[j]+b[id]*Ab[j])*(Setsize[k]+r-1)/Setsize[k];
      theta[j] = Ax[j]'/sum(Ax[j]);
	    ctg_temp += categorical_lpmf(x_slice[j]|theta[j]);
	  } 
    return ctg_temp;
  }
}
data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  int<lower=1> M;
  int<lower=1> N;
  real<lower=0> s;
  int<lower=1,upper=nPart> ID[nTrial];
  vector<lower=1,upper=M>[nTrial] Setsize;
  matrix<lower=0,upper=1>[nTrial,M] ind_mat; //indices of presented stimulus
  matrix<lower=0,upper=3.15>[nTrial,M] D; //distance of location feature, the first element is the location of the target
  matrix<lower=-pi(),upper=pi()>[M,N] E[nTrial]; //possible response errors in radian
  int<lower=1,upper=N> x[nTrial]; // responses in degree
}
parameters{
  real<lower=0,upper=1> mu_a;
  real<lower=0,upper=1> mu_b;
  real<lower=0,upper=1> mu_r;
  real<lower=0> mu_kappa;
  real<lower=0> mu_delta;
  real<lower=0> sig_1;
  real<lower=0> sig_2;
  real<lower=0> sig_3;
  
  vector<lower=0,upper=1>[nPart] a;
  vector<lower=0,upper=1>[nPart] b;
  vector<lower=0,upper=1>[nPart] r;
  vector<lower=0>[nPart] kappa;
  vector<lower=0>[nPart] delta;
}
transformed parameters{
  vector<lower=0>[nPart] kappaf=kappa+delta;
}
model{
  int grainsize = 1;
  mu_a ~ beta(1,1);
  mu_b ~ beta(1,1);
  mu_r ~ beta(1,1);
  mu_kappa ~ normal(10,5);
  mu_delta ~ normal(0,20);
  sig_1 ~ cauchy(0,1);
  sig_2 ~ cauchy(0,1);
  sig_3 ~ cauchy(0,1);
  //mu_kappaf ~ normal(30,4);
  
  for(i in 1:nPart){
    a[i] ~ normal(mu_a,sig_1);
    b[i] ~ normal(mu_b,sig_1);
    r[i] ~ normal(mu_r,sig_2);
    kappa[i] ~ normal(mu_kappa,sig_3);
    delta[i] ~ normal(mu_delta,sig_3);
  }
  
  //likelihood
  target += reduce_sum(partial_sum, x , grainsize, ID, M, N, Setsize, ind_mat,D, E, a, b, r, kappa, kappaf,s);
}
