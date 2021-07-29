functions{
  real partial_sum(int[,] y_slice, int start, int end,int n, vector beta_i_xo,vector beta_i_xa,vector beta_i_xr,vector beta_i_po,vector beta_i_pa,vector beta_i_pr,vector beta_i_to,vector beta_i_ta,vector beta_i_tr, vector xs,vector xd,vector xr, vector ps,vector pd,vector pr, vector ts,vector td,vector tr) {
    real binom_temp=0.0;
    int len = end-start+1;
  	int nTrial = num_elements(xs);
  	int k;
  	vector[nTrial] X[len];
  	vector[nTrial] R[len];
  	vector[nTrial] TT[len];
  	vector[nTrial] theta_logit[len];
	
    for(j in 1:len){
  	  k = start+j-1;
  	  X[j] = beta_i_xo[k]*xs+beta_i_xa[k]*xd+beta_i_xr[k]*xr;
  	  R[j] = beta_i_po[k]*ps+beta_i_pa[k]*pd+beta_i_pr[k]*pr;
  	  TT[j] = beta_i_to[k]*ts+beta_i_ta[k]*td+beta_i_tr[k]*tr;
  	  theta_logit[j] = fmin(fmax(X[j]+R[j]+TT[j],-10),10);
      binom_temp += binomial_logit_lpmf(y_slice[j]|n,theta_logit[j]);
    }
    return binom_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> n;
  int<lower=1> nsim;
  vector<lower=-1,upper=1>[nTrial] xs;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] td;
  vector[nTrial] tr;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] pd;
  vector[nTrial] pr;
  int<lower=0> k[nsim,nTrial];
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
  
  vector<lower=0>[nsim] beta_i_xo;
  vector<lower=0>[nsim] beta_i_to;
  vector<lower=0>[nsim] beta_i_po;
  vector<lower=0>[nsim] beta_i_xa;
  vector<lower=0>[nsim] beta_i_ta;
  vector<lower=0>[nsim] beta_i_xr;
  vector<lower=0>[nsim] beta_i_tr;
  vector<lower=0>[nsim] beta_i_pa;
  vector<lower=0>[nsim] beta_i_pr;
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
  
  beta_i_xo ~ normal(beta_xo,1);
  beta_i_po ~ normal(beta_po,1);
  beta_i_to ~ normal(beta_to,1);
  beta_i_xa ~ normal(beta_xa,1);
  beta_i_xr ~ normal(beta_xr,1);
  beta_i_pa ~ normal(beta_pa,1);
  beta_i_pr ~ normal(beta_pr,1);
  beta_i_ta ~ normal(beta_ta,1);
  beta_i_tr ~ normal(beta_tr,1);
  
  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,n,beta_i_xo,beta_i_xa,beta_i_xr,beta_i_po,beta_i_pa,beta_i_pr,beta_i_to,beta_i_ta,beta_i_tr,xs,xd,xr,ps,pd,pr,ts,td,tr);
}
