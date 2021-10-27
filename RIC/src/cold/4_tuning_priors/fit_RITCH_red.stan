functions{
  real partial_sum(real[] p_slice, int start, int end, int[] Exp, vector xs, vector xd, vector xr, vector ps, vector pd, vector pr, vector ts, vector td, vector tr, vector beta_i_xo, vector beta_i_xa, vector beta_i_xr, vector beta_i_po, vector beta_i_pa, vector beta_i_pr, vector beta_i_to, vector beta_i_ta, vector beta_i_tr) {
    int len = end-start+1;
    int k;
    int Eid;
  	vector[len] X;
    vector[len] TT;
    vector[len] R;
    real theta[len];    
    real norm_temp;
	
	for(j in 1:len){
	  k = start+j-1;
	  Eid = Exp[k]; //index of study
	  X[j] = beta_i_xo[Eid]*xs[k]+beta_i_xa[Eid]*xd[k]+beta_i_xr[Eid]*xr[k];
	  TT[j] = beta_i_to[Eid]*ts[k]+beta_i_ta[Eid]*td[k]+beta_i_tr[Eid]*tr[k];
	  R[j] = beta_i_po[Eid]*ps[k]+beta_i_pa[Eid]*pd[k]+beta_i_pr[Eid]*pr[k];
    }
	theta = to_array_1d(inv_logit(X+TT+R));
    norm_temp += normal_lpdf(p_slice|theta,0.1);
    return norm_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> nExp;
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
  real<lower=0> p[nTrial];
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
  vector<lower=0>[nExp] sd_i = rep_vector(1,nExp);
}
model{
  int grainsize=1;
  //priors of group parameters
  beta_xo ~ normal(0,1);
  beta_po ~ normal(0,1);
  beta_to ~ normal(0,1);
  beta_xa ~ normal(0,1);
  beta_xr ~ normal(0,1);
  beta_pa ~ normal(0,1);
  beta_pr ~ normal(0,1);
  beta_ta ~ normal(0,1);
  beta_tr ~ normal(0,1);
  
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
  target += reduce_sum(partial_sum,p,grainsize,Exp,xs,xd,xr,ps,pd,pr,ts,td,tr,beta_i_xo,beta_i_xa,beta_i_xr,beta_i_po,beta_i_pa,beta_i_pr,beta_i_to,beta_i_ta,beta_i_tr);
}
