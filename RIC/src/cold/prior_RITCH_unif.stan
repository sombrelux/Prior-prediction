functions{
  real trunc_normal_rng(real mu, real sigma, real lb, real ub) {
    real p_lb = 0;
    real p_ub = 1;
    real u;
    real y;
    if(!is_inf(ub)){
      p_ub = normal_cdf(ub,mu,sigma);
    }
    if(!is_inf(lb)){
      p_lb = normal_cdf(lb,mu,sigma);
    }
    u = uniform_rng(p_lb,p_ub);
    y = mu + sigma * inv_Phi(u);
    return y;
  }
}
data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  row_vector<lower=-1,upper=1>[nTrial] xs;
  row_vector[nTrial] xd;
  row_vector[nTrial] xr;
  row_vector<lower=-1,upper=1>[nTrial] ps;
  row_vector[nTrial] pd;
  row_vector[nTrial] pr;
  row_vector<lower=-1,upper=1>[nTrial] ts;
  row_vector[nTrial] td;
  row_vector[nTrial] tr;
  
  real mu_beta_xt;
  real mu_beta_xp;
  real<lower=0> mu_beta_xa;
  real<lower=0> mu_beta_xr;
  real<lower=0> mu_beta_pa;
  real<lower=0> mu_beta_pr;
  real<lower=0> mu_beta_ta;
  real<lower=0> mu_beta_tr;
  real<lower=0> sig_beta_xt;
  real<lower=0> sig_beta_xp;
  real<lower=0> sig_beta_xa;
  real<lower=0> sig_beta_xr;
  real<lower=0> sig_beta_pa;
  real<lower=0> sig_beta_pr;
  real<lower=0> sig_beta_ta;
  real<lower=0> sig_beta_tr;
  real<lower=0> Ub_to;
}
generated quantities{
  vector[nPart] beta_xp;
  vector[nPart] beta_xt;
  vector<lower=0>[nPart] beta_xo;
  vector<lower=0>[nPart] beta_to;
  vector<lower=0>[nPart] beta_po;
  vector<lower=0>[nPart] beta_xa;
  vector<lower=0>[nPart] beta_pa;
  vector<lower=0>[nPart] beta_xr;
  vector<lower=0>[nPart] beta_pr;
  vector<lower=0>[nPart] beta_ta;
  vector<lower=0>[nPart] beta_tr;
  matrix[nPart,nTrial] X;
  matrix[nPart,nTrial] R;
  matrix[nPart,nTrial] TT;
  array[nTrial,nPart] real theta_logit;
  array[nTrial,nPart] int<lower=0,upper=1> ypred;
  
  for(k in 1:nPart){
    beta_to[k] = uniform_rng(0,Ub_to);
    beta_xp[k] = normal_rng(mu_beta_xp,sig_beta_xp);
    beta_xt[k] = normal_rng(mu_beta_xt,sig_beta_xt);
    beta_xa[k] = trunc_normal_rng(mu_beta_xa,sig_beta_xa,0,positive_infinity());
    beta_xr[k] = trunc_normal_rng(mu_beta_xr,sig_beta_xr,0,positive_infinity());
    beta_pa[k] = trunc_normal_rng(mu_beta_pa,sig_beta_pa,0,positive_infinity());
    beta_pr[k] = trunc_normal_rng(mu_beta_pr,sig_beta_pr,0,positive_infinity());
    beta_ta[k] = trunc_normal_rng(mu_beta_ta,sig_beta_ta,0,positive_infinity());
    beta_tr[k] = trunc_normal_rng(mu_beta_tr,sig_beta_tr,0,positive_infinity());
	
	  beta_xo[k] = fmax(beta_to[k]-beta_xt[k],0);
	  beta_po[k] = fmax(beta_xo[k]+beta_xp[k],0);
    
    X[k] = beta_xo[k]*xs+beta_xa[k]*xd+beta_xr[k]*xr;
    R[k] = beta_po[k]*ps+beta_pa[k]*pd+beta_pr[k]*pr;
    TT[k] = beta_to[k]*ts+beta_ta[k]*td+beta_tr[k]*tr;
  }
  theta_logit = to_array_2d((X+R+TT)');
  for(j in 1:nTrial){
    ypred[j] = bernoulli_logit_rng(theta_logit[j]);
  }
}
