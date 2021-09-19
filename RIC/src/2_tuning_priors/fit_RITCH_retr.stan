functions{
  real partial_sum(int[] y_slice, int start, int end, real[] theta_logit, int[] N) {
    real binom_temp = 0.0;
    binom_temp += binomial_logit_lpmf(y_slice|N[start:end],theta_logit[start:end]);
    return binom_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> nExp;
  int<lower=1> N[nTrial];
  int<lower=1,upper=nExp> Exp[nTrial];
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
  real<lower=0> beta_xr;
  real<lower=0> beta_tr;
  real<lower=0> beta_pa;
  real<lower=0> beta_pr;
  
  vector<lower=0>[nExp] beta_i_xo;
  vector<lower=0>[nExp] beta_i_to;
  vector<lower=0>[nExp] beta_i_po;
  vector<lower=0>[nExp] beta_i_xa;
  vector<lower=0>[nExp] beta_i_ta;
  vector<lower=0>[nExp] beta_i_xr;
  vector<lower=0>[nExp] beta_i_tr;
  vector<lower=0>[nExp] beta_i_pa;
  vector<lower=0>[nExp] beta_i_pr;
}
transformed parameters{
  vector<lower=0>[nExp] sd_i = rep_vector(10,nExp);
  vector[nTrial] X;
  vector[nTrial] TT;
  vector[nTrial] R;
  real theta_logit[nTrial];   
	for(j in 1:nTrial){
	  X[j] = beta_i_xo[Exp[j]]*xs[j]+beta_i_xa[Exp[j]]*xd[j]+beta_i_xr[Exp[j]]*xr[j];
	  TT[j] = beta_i_to[Exp[j]]*ts[j]+beta_i_ta[Exp[j]]*td[j]+beta_i_tr[Exp[j]]*tr[j];
	  R[j] = beta_i_po[Exp[j]]*ps[j]+beta_i_pa[Exp[j]]*pd[j]+beta_i_pr[Exp[j]]*pr[j];
  }
  theta_logit = to_array_1d(fmax(fmin(X+TT+R,10),-10));
}
model{
  int grainsize=1;
  //priors of group parameters
  beta_xo ~ normal(15,10);
  beta_po ~ normal(11,10);
  beta_to ~ normal(7,10);
  beta_xa ~ normal(2,10);
  beta_xr ~ normal(2,10);
  beta_pa ~ normal(6,10);
  beta_pr ~ normal(6,10);
  beta_ta ~ normal(2,10);
  beta_tr ~ normal(5,10);
  
  //priors of individual parameters
  beta_i_xo ~ normal(beta_xo,sd_i);
  beta_i_po ~ normal(beta_po,sd_i);
  beta_i_to ~ normal(beta_to,sd_i);
  beta_i_xa ~ normal(beta_xa,sd_i);
  beta_i_xr ~ normal(beta_xr,sd_i);
  beta_i_pa ~ normal(beta_pa,sd_i);
  beta_i_pr ~ normal(beta_pr,sd_i);
  beta_i_ta ~ normal(beta_ta,sd_i);
  beta_i_tr ~ normal(beta_tr,sd_i);
  
  //likelihood
  target += reduce_sum(partial_sum,y,grainsize,theta_logit,N);
}
