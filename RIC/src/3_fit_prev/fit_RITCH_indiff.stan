data{
  int<lower=1> nTrial;
  vector<lower=-1,upper=1>[nTrial] xs;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector[nTrial] td;
  vector[nTrial] tr;
  vector[nTrial] pd;
  vector[nTrial] pr;
  real<lower=0> sigma;
}
parameters{
  real<lower=0> beta_xo;
  real<lower=0> beta_to;
  real<lower=0> beta_po;
  real<lower=0> beta_xa;
  real<lower=0> beta_xr;
  real<lower=0> beta_ta;
  real<lower=0> beta_tr;
  real<lower=0> beta_pa;
  real<lower=0> beta_pr;
}
transformed parameters{
  vector[nTrial] X;
  vector[nTrial] TT;
  vector[nTrial] R;
  vector<lower=0,upper=1>[nTrial] theta;
  
  X = beta_xo*xs+beta_xa*xd+beta_xr*xr;
  TT = beta_to*ts+beta_ta*td+beta_tr*tr;
  R = beta_po*ps+beta_pa*pd+beta_pr*pr;
  theta = inv_logit(fmin(fmax(X+TT+R,-20),20));
}
model{
  //priors
  beta_xo ~ normal(0,1);
  beta_to ~ normal(0,1);
  beta_po ~ normal(0,1);
  beta_xa ~ normal(0,1);
  beta_xr ~ normal(0,1);
  beta_pa ~ normal(0,1);
  beta_pr ~ normal(0,1);
  beta_ta ~ normal(0,1);
  beta_tr ~ normal(0,1);
  
  //likelihood
  target += normal_lpdf(theta|0.5, sigma);
}
