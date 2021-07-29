functions{
  real partial_sum(real[] k_slice, int start, int end, real[] theta) {
    real norm_temp=0.0;
    int  len = end-start+1;
    for(j in 1:len){
      norm_temp += normal_lpdf(k_slice[j]|theta[start+j-1],1);
    }
    return norm_temp;
  }
}
data{
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o2;
  real<lower=0,upper=1> k[nTrial];
}
parameters{
  //group parameters
  real<lower=0,upper=1> a;
  real logh;
  real<lower=0> i;
  real<lower=0> s;
}
transformed parameters{
  real<lower=0> h = exp(logh);
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=1>[nTrial] invw2;
  vector<lower=0>[nTrial] U2;
  real<lower=0,upper=1> theta[nTrial];
  
  v1 = pow(x1,a);
  v2 = pow(x2,a);
  
  invw2 = 1+h*(t2+i*o2);
  U2 = v2./invw2;
      
  theta = to_array_1d(inv_logit(s*(v1-U2)));
}
model{
  int grainsize=1;
  //priors

  a ~ beta(1,4);
  logh ~ normal(-4,2);
  i ~ normal(50,10);
  s ~ normal(4,2);

  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta);
}
