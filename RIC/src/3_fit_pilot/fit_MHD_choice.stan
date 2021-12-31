data{
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
  int<lower=1> N;
  array[nTrial] int<lower=0> y;
}
parameters{
  real<lower=0,upper=2> a;
  real<lower=0,upper=1> c;
  real<upper=0> logh_d;
  real<lower=0> logh_r;
  real<lower=0,upper=1> s_d;
  real<lower=0,upper=1> s_r;
  real<lower=0> s;
}
transformed parameters{
  real<lower=0> h_d = exp(logh_d);
  real<lower=0> h_r = exp(logh_r);
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector<upper=0>[nTrial] logw1;
  vector<upper=0>[nTrial] logw2;
  vector<upper=0>[nTrial] logd1;
  vector<upper=0>[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  array[nTrial] real theta_logit;
  
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  logw1 = -s_r*pow(x1,c).*log1p(h_r*o1);
  logw2 = -s_r*pow(x2,c).*log1p(h_r*o2);
  logd1 = -s_d*log1p(h_d*t1);
  logd2 = -s_d*log1p(h_d*t2);
  
  U1 = exp(logv1+logd1+logw1);
  U2 = exp(logv2+logd2+logw2);
  theta_logit = to_array_1d(s*(U1-U2));
}
model{
  //priors
  a ~ normal(0,1);
  c ~ normal(0,1);
  logh_r ~ normal(1,1);
  logh_d ~ normal(-1,1);
  s_r ~ normal(0,1);
  s_d ~ normal(0,1);
  s ~ normal(0,1);
  
  //likelihood
  target += binomial_logit_lpmf(y|N,theta_logit);
}
generated quantities{
  array[nTrial] int<lower=0> ypred;
  array[nTrial] real<lower=0,upper=1> theta;
  theta  = inv_logit(theta_logit);
  ypred = binomial_rng(N,theta);
}
