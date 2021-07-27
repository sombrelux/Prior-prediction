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
  vector[nTrial] rva_ind;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector[nTrial] pd;
  vector[nTrial] pr;
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
  
  theta_logit = to_array_1d(fmax(fmin(X+R+TT+beta_rva*rva_ind+beta_dva*(1-rva_ind),-10),10));
}
model{
  int grainsize=1;
  //priors
  beta_rva ~ normal(0,1);
  beta_dva ~ normal(0,1);
  beta_xa ~ normal(0,1);
  beta_xr ~ normal(0,1);
  beta_pa ~ normal(0,1);
  beta_pr ~ normal(0,1);
  beta_ta ~ normal(0,1);
  beta_tr ~ normal(0,1);
  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta_logit,n);
}
