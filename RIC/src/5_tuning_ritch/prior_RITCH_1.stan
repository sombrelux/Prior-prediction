functions{
  real trunc_normal_rng(real mu, real sigma, real lb, real ub) {
    real p_lb = normal_cdf(lb,mu,sigma);
    real p_ub = 1;
    real u;
    real y;
    if(!is_inf(ub)){
      p_ub = normal_cdf(ub,mu,sigma);
    }
    u = uniform_rng(p_lb,p_ub);
    y = mu + sigma * inv_Phi(u);
    return y;
  }
}
data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  vector<lower=-1,upper=1>[nTrial] xs;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector[nTrial] pd;
  vector[nTrial] pr;
  vector[nTrial] td;
  vector[nTrial] tr;
}
generated quantities{
  vector<lower=0>[nPart] beta_xo;
  vector<lower=0>[nPart] beta_to;
  vector<lower=0>[nPart] beta_po;
  vector<lower=0>[nPart] beta_xa;
  vector<lower=0>[nPart] beta_pa;
  vector<lower=0>[nPart] beta_xr;
  vector<lower=0>[nPart] beta_pr;
  vector<lower=0>[nPart] beta_ta;
  vector<lower=0>[nPart] beta_tr;
  vector[nTrial] X[nPart];
  vector[nTrial] R[nPart];
  vector[nTrial] TT[nPart];
  real theta_logit[nPart,nTrial];
  int<lower=0,upper=1> ypred[nPart,nTrial];
  
  for(k in 1:nPart){
    beta_xo[k] = trunc_normal_rng(0,1,0,positive_infinity());
    beta_to[k] = trunc_normal_rng(0.5,0.5,0,positive_infinity());
    beta_po[k] = trunc_normal_rng(0.1,0.1,0,positive_infinity());
    
    beta_xa[k] = trunc_normal_rng(0.005,0.02,0,positive_infinity());
    beta_xr[k] = trunc_normal_rng(2,1,0,positive_infinity());
    beta_pa[k] = trunc_normal_rng(2,1,0,positive_infinity());
    beta_pr[k] = trunc_normal_rng(2,1,0,positive_infinity());
    beta_ta[k] = trunc_normal_rng(1,1,0,positive_infinity());
    beta_tr[k] = trunc_normal_rng(0.5,0.5,0,positive_infinity());
    
    X[k] = beta_xo[k]*xs+beta_xa[k]*xd+beta_xr[k]*xr;
    R[k] = beta_po[k]*ps+beta_pa[k]*pd+beta_pr[k]*pr;
    TT[k] = beta_to[k]*ts+beta_ta[k]*td+beta_tr[k]*tr;
    
    theta_logit[k] = to_array_1d(X[k]+R[k]+TT[k]);
    for(i in 1:nTrial){
      ypred[k,i] = bernoulli_logit_rng(theta_logit[k,i]);
    }
  }
}
