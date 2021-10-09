data{
  int<lower=1> nTrial;
  int<lower=1> N[nTrial];
  vector<lower=-1,upper=1>[nTrial] xs;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] td;
  vector[nTrial] tr;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] pd;
  vector[nTrial] pr;
  int<lower=0> y[nTrial];
}
parameters{
  real<lower=0> beta_xo;
  real<lower=0> beta_to;
  real<lower=0> beta_po;
  real<lower=0> beta_xa;
  real<lower=0> beta_ta;
  real<lower=0> beta_pa;
  real<lower=0> beta_xr;
  real<lower=0> beta_tr;
  real<lower=0> beta_pr;
}
transformed parameters{
  vector[nTrial] X;
  vector[nTrial] TT;
  vector[nTrial] R;
  real theta_logit[nTrial]; 
  for(j in 1:nTrial){
	X[j] = beta_xo*xs[j]+beta_xa*xd[j]+beta_xr*xr[j];
	TT[j] = beta_to*ts[j]+beta_ta*td[j]+beta_tr*tr[j];
	R[j] = beta_po*ps[j]+beta_pa*pd[j]+beta_pr*pr[j];
  }
  theta_logit = to_array_1d(fmax(fmin(X+TT+R,20),-20));
}
model{
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
  target += binomial_logit_lpmf(y|N,theta_logit);
}
generated quantities{
  real<lower=0,upper=1> theta[nTrial]; 
  int<lower=0> ypred[nTrial];
  theta = inv_logit(theta_logit);
  ypred = binomial_rng(N,theta);
}
