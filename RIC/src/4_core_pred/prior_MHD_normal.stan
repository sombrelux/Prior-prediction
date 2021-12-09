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
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] o2;
  vector<lower=0>[nTrial] t2;
  
  real<lower=0> mu_a;
  real<lower=0> mu_c;
  real<lower=0> mu_s;
  real mu_loghd;
  real mu_logsd;
  real mu_loghr;
  real mu_logsr;
  real<lower=0> sig_a;
  real<lower=0> sig_c;
  real<lower=0> sig_s;
  real<lower=0> sig_loghd;
  real<lower=0> sig_logsd;
  real<lower=0> sig_loghr;
  real<lower=0> sig_logsr;
}
generated quantities{
  real<lower=0,upper=2> a;
  real<lower=0,upper=1> c;
  real<lower=0> s;
  real<lower=0> logh_r;
  real<upper=0> logh_d;
  real<upper=0> logs_r;
  real<upper=0> logs_d;
  real<lower=1> h_r;
  real<lower=0,upper=1> h_d;
  real<lower=0,upper=1> s_r;
  real<lower=0,upper=1> s_d;
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector<upper=0>[nTrial] logw1;
  vector<upper=0>[nTrial] logw2;
  vector<upper=0>[nTrial] logd1;
  vector<upper=0>[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  array[nTrial] real<lower=0,upper=1> theta;
  array[nTrial] int<lower=0,upper=nPart> ypred;
  
  a = trunc_normal_rng(mu_a,sig_a,0,2);
  c = trunc_normal_rng(mu_c,sig_c,0,1);
  s = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());
  logh_d = trunc_normal_rng(mu_loghd,sig_loghd,negative_infinity(),0);
  logh_r = trunc_normal_rng(mu_loghr,sig_loghr,0,positive_infinity());
  logs_d = trunc_normal_rng(mu_logsd,sig_logsd,negative_infinity(),0);
  logs_r = trunc_normal_rng(mu_logsr,sig_logsr,negative_infinity(),0);

  h_r = exp(logh_r);
  s_r = exp(logs_r);
  h_d = exp(logh_d);
  s_d = exp(logs_d);

  logw1 = -s_r*pow(x1,c).*log1p(h_r*o1);
  logw2 = -s_r*pow(x2,c).*log1p(h_r*o2);
  
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  
  logd1 = -s_d*log1p(h_d*t1);
  logd2 = -s_d*log1p(h_d*t2);
  
  U1 = exp(logv1+logw1+logd1);
  U2 = exp(logv2+logw2+logd2);
  theta = to_array_1d(inv_logit(s*(U1-U2)));
  for(j in 1:nTrial){
    ypred[j] = binomial_rng(nPart,theta[j]);
  }
}
