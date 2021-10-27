data{
  int<lower=1> nTrial;
  vector<lower=-1,upper=1>[nTrial] xs;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] td;
  vector[nTrial] tr;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] pd;
  vector[nTrial] pr;
  array[nTrial] int<lower=1> N;
  array[nTrial] int<lower=0> y;
}
parameters{
  real<lower=0> beta_xo;
  real<lower=0> beta_to;
  real<lower=0> beta_po;
  real<lower=0> beta_xa;
  real<lower=0> beta_ta;
  real<lower=0> beta_xr;
  real<lower=0> beta_tr;
  real<lower=0> beta_pa;
  real<lower=0> beta_pr;
}
transformed parameters{
  vector[nTrial] X;
  vector[nTrial] TT;
  vector[nTrial] R;
  array[nTrial] real theta_logit;
  
  X = beta_xo*xs+beta_xa*xd+beta_xr*xr;
  TT = beta_to*ts+beta_ta*td+beta_tr*tr;
  R = beta_po*ps+beta_pa*pd+beta_pr*pr;
  theta_logit = to_array_1d(fmin(fmax(X+TT+R,-10),10));
}
model{
  //priors
  beta_xo ~ normal(0,1);
  beta_po ~ normal(0,1);
  beta_to ~ normal(0,1);
  beta_xa ~ normal(0,1);
  beta_xr ~ normal(0,1);
  beta_pa ~ normal(0,1);
  beta_pr ~ normal(0,1);
  beta_ta ~ normal(0,1);
  beta_tr ~ normal(0,1);
  
  //likelihood
  target += binomial_logit_lpmf(y|N,theta_logit);
}
generated quantities{
  array[nTrial] int<lower=0> ypred;
  array[nTrial] real<lower=0,upper=1> theta;
  theta  = inv_logit(theta_logit);
  ypred = binomial_rng(N,theta);
}
