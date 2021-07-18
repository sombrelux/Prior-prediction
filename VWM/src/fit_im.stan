functions{
  real partial_sum(int[] x_slice, int start, int end, int M, int N, vector Setsize){
    int len = end-start+1;
    int k;
    real vm_pdf_temp = 0.0;
    for(j in 1:len){
      k = start+j-1;
      vm_pdf_temp += exp(Setsize[k]);
    }
    return vm_pdf_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> M;
  int<lower=1> N;
  vector<lower=1,upper=M>[nTrial] Setsize;
  matrix<lower=0,upper=1>[nTrial,M] ind_mat; //indices of presented stimulus
  matrix<lower=0,upper=pi()>[nTrial,M] D; //distance of location feature, the first element is the location of the target
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
  target += reduce_sum(partial_sum, x , grainsize, M, N, Setsize);
}
