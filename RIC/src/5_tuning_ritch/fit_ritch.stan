functions{
  real partial_sum(int[] y_slice, int start, int end, real[] theta_logit,int n) {
    real binom_temp=0.0;
    int  len = end-start+1;
    for(j in 1:len){
        binom_temp += binomial_logit_lpmf(y_slice[j]|n,theta_logit[start+j-1]);
    }
    return binom_temp;
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
  int<lower=0> n;
  int<lower=0> k[nTrial];
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
  real theta_logit[nTrial];
  X = beta_xo*xs+beta_xa*xd+beta_xr*xr;
  TT = beta_to*ts+beta_ta*td+beta_tr*tr;
  R = beta_po*ps+beta_pa*pd+beta_pr*pr;
  theta_logit = to_array_1d(X+TT+R);
}
model{
  int grainsize=1;
  //priors
  beta_xo ~ normal(0,1);
  beta_po ~ normal(0,1);
  beta_to ~ normal(0,1);
  beta_xa ~ normal(1,1);
  beta_xr ~ normal(1,1);
  beta_pa ~ normal(1,1);
  beta_pr ~ normal(1,1);
  beta_ta ~ normal(1,1);
  beta_tr ~ normal(1,1);
  
  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta_logit,n);
}
