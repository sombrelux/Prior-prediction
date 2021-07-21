functions{
  real partial_sum(int[] k_slice, int start, int end, real[] theta_logit,int[] n) {
    real binom_temp=0.0;
    int  len = end-start+1;
    for(j in 1:len){
        binom_temp += binomial_logit_lpmf(k_slice[j]|n[start+j-1],theta_logit[start+j-1]);
    }
    return binom_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> n[nTrial];
  int<lower=1> N;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] pd;
  vector[nTrial] pr;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] td;
  vector[nTrial] tr;
  
  int<lower=0,upper=N> k[nTrial];

}
parameters{
  //group parameters
  real beta_dva;
  real beta_rva;
  real<lower=0> beta_xa;
  real<lower=0> beta_pa;
  real<lower=0> beta_xr;
  real<lower=0> beta_pr;
  real<lower=0> beta_ta;
  real<lower=0> beta_tr;
}
transformed parameters{
  vector[nTrial] X;
  vector[nTrial] R;
  vector[nTrial] TT;
  real theta_logit[nTrial];
  
  X = beta_xa*xd+beta_xr*xr;
  R = beta_pa*pd+beta_pr*pr;
  TT = beta_ta*td+beta_tr*tr;
  
  theta_logit = X+R+TT+beta_rva
}
model{
  int grainsize=1;
  //priors
  beta_o ~ normal(1,10);
  beta_xa ~ normal(1,10);
  beta_xr ~ normal(1,10);
  beta_pa ~ normal(1,10);
  beta_pr ~ normal(1,10);
  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta_logit,nPart);
}
generated quantities{
  int<lower=0> kpred[nTrial];

  for(i in 1:nTrial){
    kpred[i] = binomial_rng(nPart,inv_logit(theta_logit[i]));
  }
}
