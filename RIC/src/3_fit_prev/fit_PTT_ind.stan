data{
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0,upper=1>[nTrial] p1;
  vector<lower=0,upper=1>[nTrial] p2;
  array[nTrial] int<lower=1> N;
  array[nTrial] int<lower=0> y;
}
parameters{
  //group parameters
  real<lower=10^(-5)> alpha;
  real<upper=1> beta;
  real<lower=0,upper=1> gamma;
  real<lower=0> R;
  real<lower=0> s;
}
transformed parameters{
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=0,upper=1>[nTrial] w1;
  vector<lower=0,upper=1>[nTrial] w2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  array[nTrial] real theta_logit;

  v1 = (1-exp(-alpha*pow(x1,1-beta)))/alpha;
  v2 = (1-exp(-alpha*pow(x2,1-beta)))/alpha;
  
  w1 = exp(-pow(R*t1./x1-log(p1),gamma));
  w2 = exp(-pow(R*t2./x2-log(p2),gamma));
  
  U1 = v1.*w1;
  U2 = v2.*w2;

  theta_logit = to_array_1d(s*(U1-U2));
}
model{
  //priors
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  gamma ~ beta(1,1);
  R ~ normal(0,1);
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
