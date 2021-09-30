functions{
  real partial_sum(int[] y_slice, int start, int end, real[] theta_logit, int[] N) {
    real binom_temp = 0.0;
    binom_temp += binomial_logit_lpmf(y_slice|N[start:end],theta_logit[start:end]);
    return binom_temp;
  }
}
data{
  int<lower=1> nExp;
  int<lower=1> nTrial;
  int<lower=1> Exp[nTrial];
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
  
  real<lower=0> sd_i;
  vector<lower=0>[nExp] beta_xo_i;
  vector<lower=0>[nExp] beta_to_i;
  vector<lower=0>[nExp] beta_po_i;
  vector<lower=0>[nExp] beta_xa_i;
  vector<lower=0>[nExp] beta_ta_i;
  vector<lower=0>[nExp] beta_pa_i;
  vector<lower=0>[nExp] beta_xr_i;
  vector<lower=0>[nExp] beta_tr_i;
  vector<lower=0>[nExp] beta_pr_i;
}
transformed parameters{
  vector<lower=0>[nExp] SD_i = rep_vector(sd_i,nExp);
  vector[nTrial] X;
  vector[nTrial] TT;
  vector[nTrial] R;
  real theta_logit[nTrial];   
	for(j in 1:nTrial){
	  X[j] = beta_xo_i[Exp[j]]*xs[j]+beta_xa_i[Exp[j]]*xd[j]+beta_xr_i[Exp[j]]*xr[j];
	  TT[j] = beta_to_i[Exp[j]]*ts[j]+beta_ta_i[Exp[j]]*td[j]+beta_tr_i[Exp[j]]*tr[j];
	  R[j] = beta_po_i[Exp[j]]*ps[j]+beta_pa_i[Exp[j]]*pd[j]+beta_pr_i[Exp[j]]*pr[j];
  }
  theta_logit = to_array_1d(X+TT+R);
}
model{
  int grainsize=1;
  //group priors
  beta_xo ~ normal(0,1);
  beta_po ~ normal(0,1);
  beta_to ~ normal(0,1);
  beta_xa ~ normal(0.2,1);
  beta_xr ~ normal(2,1);
  beta_pa ~ normal(2,1);
  beta_pr ~ normal(1.7,1);
  beta_ta ~ normal(0.6,1);
  beta_tr ~ normal(0.4,1);
  
  //individ priors
  sd_i ~ normal(0.6,1);
  beta_xo_i ~ normal(beta_xo,SD_i);
  beta_xa_i ~ normal(beta_xa,SD_i);
  beta_xr_i ~ normal(beta_xr,SD_i);
  beta_po_i ~ normal(beta_po,SD_i);
  beta_pa_i ~ normal(beta_pa,SD_i);
  beta_pr_i ~ normal(beta_pr,SD_i);
  beta_to_i ~ normal(beta_to,SD_i);
  beta_ta_i ~ normal(beta_ta,SD_i);
  beta_tr_i ~ normal(beta_tr,SD_i);
  
  //likelihood
  target += reduce_sum(partial_sum,y,grainsize,theta_logit,N);;
}
