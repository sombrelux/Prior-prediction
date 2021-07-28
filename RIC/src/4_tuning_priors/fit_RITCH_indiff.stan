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
  vector<lower=-1,upper=1>[nTrial] xs;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] td;
  vector[nTrial] tr;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] pd;
  vector[nTrial] pr;
  real<lower=0> k[nTrial];
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
  real<lower=0,upper=1> theta[nTrial];
  X = beta_xo*xs+beta_xa*xd+beta_xr*xr;
  TT = beta_to*ts+beta_ta*td+beta_tr*tr;
  R = beta_po*ps+beta_pa*pd+beta_pr*pr;
  theta = to_array_1d(inv_logit(X+TT+R));
}
model{
  int grainsize=1;
  //priors
  beta_xo ~ normal(0,1);
  beta_po ~ normal(0.5,0.5);
  beta_to ~ normal(0.1,0.1);
  beta_xa ~ normal(0.005,0.02);
  beta_xr ~ normal(2,1);
  beta_pa ~ normal(2,1);
  beta_pr ~ normal(2,1);
  beta_ta ~ normal(1,1);
  beta_tr ~ normal(0.5,0.5);
  
  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta);
}
