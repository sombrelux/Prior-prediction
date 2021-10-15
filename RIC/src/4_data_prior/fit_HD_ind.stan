data{
  int<lower=1> nTrial;
  int<lower=1> N[nTrial];
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
  int<lower=0> y[nTrial];
}
parameters{
  real<lower=0,upper=1> a;
  real logh;
  real<lower=0> i;
  real<lower=0> s;
}
transformed parameters{
  real<lower=0> h = exp(logh);
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=1>[nTrial] invw1;
  vector<lower=1>[nTrial] invw2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  
  v1 = pow(x1,a);
  v2 = pow(x2,a);
  invw1 = 1+h*(t1+i*o1);
  invw2 = 1+h*(t2+i*o2);
  U1 = v1./invw1;
  U2 = v2./invw2;
  theta_logit = to_array_1d(s*(U1-U2));
}
model{
  //priors
  a ~ beta(1,1);
  logh ~ normal(0,1);
  i ~ normal(0,1);
  s ~ normal(0,1);
  
  //likelihood
  target += binomial_logit_lpmf(y|N,theta_logit);
}
generated quantities{
  int<lower=0> ypred[nTrial];
  real<lower=0,upper=1> theta[nTrial];
  theta  = inv_logit(theta_logit);
  ypred = binomial_rng(N,theta);
}
