data{
  int<lower=1> nTrial;
  vector[nTrial] rva_ind;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector[nTrial] pd;
  vector[nTrial] pr;
  vector[nTrial] td;
  vector[nTrial] tr;
}
generated quantities{
  real beta_dva;
  real beta_rva;
  real<lower=0> beta_xa;
  real<lower=0> beta_pa;
  real<lower=0> beta_xr;
  real<lower=0> beta_pr;
  real<lower=0> beta_ta;
  real<lower=0> beta_tr;
  vector[nTrial] X;
  vector[nTrial] R;
  vector[nTrial] TT;
  real theta_logit[nTrial];
  
  beta_rva = normal_rng(0,1);
  beta_dva = normal_rng(0,1);
  beta_xa = normal_rng(1,1);
  beta_xr = normal_rng(1,1);
  beta_pa = normal_rng(1,1);
  beta_pr = normal_rng(1,1);
  
  X = beta_xa*xd+beta_xr*xr;
  R = beta_pa*pd+beta_pr*pr;
  TT = beta_ta*td+beta_tr*tr;
  
  theta_logit = to_array_1d(X+R+TT+beta_rva*rva_ind+beta_dva*(1-rva_ind));

  for(i in 1:nTrial){
    ypred[i] = bernoulli_logit_rng(theta_logit[i]);
  }
}