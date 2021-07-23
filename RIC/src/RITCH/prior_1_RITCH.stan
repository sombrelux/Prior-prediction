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
  vector[nTrial] rva_ind;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector[nTrial] pd;
  vector[nTrial] pr;
  vector[nTrial] td;
  vector[nTrial] tr;
}
generated quantities{
  vector[nPart] beta_dva;
  vector[nPart] beta_rva;
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
    beta_rva[k] = trunc_normal_rng(0.65,0.15,0,positive_infinity());
    beta_dva[k] = trunc_normal_rng(2.2,0.1,0,positive_infinity());
    beta_xa[k] = trunc_normal_rng(0.0013,0.0002,0,positive_infinity());
    beta_xr[k] = trunc_normal_rng(2,0.1,0,positive_infinity());
    beta_pa[k] = trunc_normal_rng(2.8,0.4,0,positive_infinity());
    beta_pr[k] = trunc_normal_rng(1.4,0.2,0,positive_infinity());
    beta_ta[k] = trunc_normal_rng(0,0.05,0,positive_infinity());
    beta_tr[k] = trunc_normal_rng(0,0.02,0,positive_infinity());
    
    X[k] = beta_xa[k]*xd+beta_xr[k]*xr;
    R[k] = beta_pa[k]*pd+beta_pr[k]*pr;
    TT[k] = beta_ta[k]*td+beta_tr[k]*tr;
    
    theta_logit[k] = to_array_1d(X[k]+R[k]+TT[k]+beta_rva[k]*rva_ind+beta_dva[k]*(1-rva_ind));
  
    for(i in 1:nTrial){
      ypred[k,i] = bernoulli_logit_rng(theta_logit[k,i]);
    }
  }
}
