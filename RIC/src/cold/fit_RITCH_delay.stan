data{
  int<lower=1> nExp;
  int<lower=1> nTrial;
  int<lower=1> Exp[nTrial];
  int<lower=1> N[nTrial];
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector[nTrial] td;
  vector[nTrial] tr;
  int<lower=0> y[nTrial];
}
parameters{
  real beta_o;
  real<lower=0> beta_xa;
  real<lower=0> beta_ta;
  real<lower=0> beta_xr;
  real<lower=0> beta_tr;
  real<lower=0> sd_i;
  
  vector[nExp] beta_o_i;
  vector<lower=0>[nExp] beta_xa_i;
  vector<lower=0>[nExp] beta_ta_i;
  vector<lower=0>[nExp] beta_xr_i;
  vector<lower=0>[nExp] beta_tr_i;
}
transformed parameters{
  vector<lower=0>[nExp] SD_i = rep_vector(sd_i,nExp);
  vector[nTrial] X;
  real theta_logit[nTrial];   
  for(j in 1:nTrial){
	X[j] = beta_o_i[Exp[j]]+beta_xa_i[Exp[j]]*xd[j]+beta_xr_i[Exp[j]]*xr[j]+beta_ta_i[Exp[j]]*td[j]+beta_tr_i[Exp[j]]*tr[j];
  }
  theta_logit = to_array_1d(fmax(fmin(X,10),-10));
}
model{
  //group priors
  beta_o ~ normal(0,1);
  beta_xa ~ normal(0,1);
  beta_xr ~ normal(0,1);
  beta_ta ~ normal(0,1);
  beta_tr ~ normal(0,1);
  //individ priors
  sd_i ~ normal(0,1);
  beta_o_i ~ normal(beta_o,SD_i);
  beta_xa_i ~ normal(beta_xa,SD_i);
  beta_xr_i ~ normal(beta_xr,SD_i);
  beta_ta_i ~ normal(beta_ta,SD_i);
  beta_tr_i ~ normal(beta_tr,SD_i);
  
  //likelihood
  target += binomial_logit_lpmf(y|N,theta_logit);
}
