data{
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
}
generated quantities{
  real<lower=0> a;
  real<lower=0> s;
  real<lower=0> c;
  real<lower=0> hr;
  real<lower=0> s_r;
  real<lower=0> hd;
  real<lower=0> s_d;
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector<upper=0>[nTrial] logw1;
  vector<upper=0>[nTrial] logw2;
  vector<upper=0>[nTrial] logd1;
  vector<upper=0>[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  int<lower=0,upper=1>[nTrial] ypred;
  
  a = normal_rng(1,1);
  s = normal_rng(1,1);
  c = normal_rng(1,1);
  hr = normal_rng(1,1);
  s_r = normal_rng(1,1);
  hd = normal_rng(1,1);
  s_d = normal_rng(1,1);
  
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  
  logw1 = -s_r*pow(x1,c).*log1p(hr*o1);
  logw2 = -s_r*pow(x2,c).*log1p(hr*o2);
  
  logd1 = -s_d*log1p(hd*t1);
  logd2 = -s_d*log1p(hd*t2);
  
  U1 = exp(logv1+logd1+logw1);
  U2 = exp(logv2+logd2+logw2);
  
  theta_logit = to_array_1d(s*(U1-U2));
  
  for(i in 1:nTrial){
    ypred[i] = bernoulli_logit_rng(theta_logit[i]);
  }
}
