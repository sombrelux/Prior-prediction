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
  vector<lower=-1,upper=1>[nTrial] xs;
  vector[nTrial] xd;
  vector[nTrial] xr;
  vector<lower=-1,upper=1>[nTrial] ps;
  vector[nTrial] pd;
  vector[nTrial] pr;
  vector<lower=-1,upper=1>[nTrial] ts;
  vector[nTrial] td;
  vector[nTrial] tr;
  
  real mu_beta_xt;
  real mu_beta_xp;
  real<lower=0> mu_beta_xa;
  real<lower=0> mu_beta_xr;
  real<lower=0> mu_beta_pa;
  real<lower=0> mu_beta_pr;
  real<lower=0> mu_beta_ta;
  real<lower=0> mu_beta_tr;
  real<lower=0> sig_beta_xo;
  real<lower=0> sig_beta_xt;
  real<lower=0> sig_beta_xp;
  real<lower=0> sig_beta_xa;
  real<lower=0> sig_beta_xr;
  real<lower=0> sig_beta_pa;
  real<lower=0> sig_beta_pr;
  real<lower=0> sig_beta_ta;
  real<lower=0> sig_beta_tr;
}
generated quantities{
  real<lower=0> beta_xo;
  real<lower=0> beta_xp;
  real<lower=0> beta_xt;
  real<lower=0> beta_to;
  real<lower=0> beta_po;
  real<lower=0> beta_xa;
  real<lower=0> beta_pa;
  real<lower=0> beta_xr;
  real<lower=0> beta_pr;
  real<lower=0> beta_ta;
  real<lower=0> beta_tr;
  vector[nTrial] X;
  vector[nTrial] R;
  vector[nTrial] TT;
  array[nTrial] real<lower=0,upper=1> theta;
  array[nTrial] int<lower=0,upper=nPart> ypred;
  
  beta_xo = trunc_normal_rng(0,sig_beta_xo,0,positive_infinity());
  beta_xp = trunc_normal_rng(mu_beta_xp,sig_beta_xp,0,positive_infinity());
  beta_xt = trunc_normal_rng(mu_beta_xt,sig_beta_xt,0,positive_infinity());
  beta_xa = trunc_normal_rng(mu_beta_xa,sig_beta_xa,0,positive_infinity());
  beta_xr = trunc_normal_rng(mu_beta_xr,sig_beta_xr,0,positive_infinity());
  beta_pa = trunc_normal_rng(mu_beta_pa,sig_beta_pa,0,positive_infinity());
  beta_pr = trunc_normal_rng(mu_beta_pr,sig_beta_pr,0,positive_infinity());
  beta_ta = trunc_normal_rng(mu_beta_ta,sig_beta_ta,0,positive_infinity());
  beta_tr = trunc_normal_rng(mu_beta_tr,sig_beta_tr,0,positive_infinity());
  beta_to = beta_xo+beta_xt;
  beta_po = beta_xo+beta_xp;

  X = beta_xo*xs+beta_xa*xd+beta_xr*xr;
  R = beta_po*ps+beta_pa*pd+beta_pr*pr;
  TT = beta_to*ts+beta_ta*td+beta_tr*tr;
  
  theta = to_array_1d(inv_logit(X+R+TT));
  for(j in 1:nTrial){
    ypred[j] = binomial_rng(nPart,theta[j]);
  }
}
