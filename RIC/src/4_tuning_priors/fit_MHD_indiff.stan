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
  real<lower=0,upper=1> c;
  real<lower=0> loghr;
  real<upper=0> loghd;
  real<lower=0,upper=1> s_r;
  real<lower=0,upper=1> s_d;
  real<lower=0> s;
}
transformed parameters{
  real<lower=0> hr = exp(loghr);
  real<lower=0> hd = exp(loghd);
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector<upper=0>[nTrial] logw2;
  vector<upper=0>[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real<lower=0,upper=1> theta[nTrial];
  
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  
  logw2 = -s_r*pow(x2,c).*log1p(hr*o2);
  logd2 = -s_d*log1p(hd*t2);
  
  U1 = exp(logv1);
  U2 = exp(logv2+logd2+logw2);
  
  theta = to_array_1d(inv_logit(s*(U1-U2)));
}
model{
  int grainsize=1;
  //priors
  a ~ beta(1,3);
  c ~ beta(1,5);
  loghr ~ normal(1,1);
  loghd ~ normal(-1,1);
  s_r ~ beta(2,2);
  s_d ~ beta(1,3);
  s ~ normal(5,2);

  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta);
}
