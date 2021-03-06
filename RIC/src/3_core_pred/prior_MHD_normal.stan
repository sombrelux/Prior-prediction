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
  row_vector<lower=0>[nTrial] x1;
  row_vector<lower=0>[nTrial] o1;
  row_vector<lower=0>[nTrial] t1;
  row_vector<lower=0>[nTrial] x2;
  row_vector<lower=0>[nTrial] o2;
  row_vector<lower=0>[nTrial] t2;
  
  real<lower=0> mu_a;
  real<lower=0> mu_c;
  real<lower=0> mu_s;
  real mu_loghd;
  real mu_sd;
  real mu_loghr;
  real mu_sr;
  real<lower=0> sig_a;
  real<lower=0> sig_c;
  real<lower=0> sig_s;
  real<lower=0> sig_loghd;
  real<lower=0> sig_sd;
  real<lower=0> sig_loghr;
  real<lower=0> sig_sr;
}
generated quantities{
  vector<lower=0,upper=2>[nPart] a;
  vector<lower=0,upper=1>[nPart] c;
  vector<lower=0>[nPart] s;
  vector<lower=0>[nPart] logh_r;
  vector<upper=0>[nPart] logh_d;
  vector<lower=1>[nPart] h_r;
  vector<lower=0,upper=1>[nPart] h_d;
  vector<lower=0,upper=1>[nPart] s_r;
  vector<lower=0,upper=1>[nPart] s_d;
  matrix[nPart,nTrial] logv1;
  matrix[nPart,nTrial] logv2;
  matrix<upper=0>[nPart,nTrial] logw1;
  matrix<upper=0>[nPart,nTrial] logw2;
  matrix<upper=0>[nPart,nTrial] logd1;
  matrix<upper=0>[nPart,nTrial] logd2;
  matrix<lower=0>[nPart,nTrial] U1;
  matrix<lower=0>[nPart,nTrial] U2;
  array[nTrial,nPart] real theta_logit;
  array[nTrial,nPart] int<lower=0,upper=1> ypred;
  
  for(k in 1:nPart){
    a[k] = trunc_normal_rng(mu_a,sig_a,0,2);
  	c[k] = trunc_normal_rng(mu_c,sig_c,0,1);
    s[k] = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());
    logh_d[k] = trunc_normal_rng(mu_loghd,sig_loghd,negative_infinity(),0);
    logh_r[k] = trunc_normal_rng(mu_loghr,sig_loghr,0,positive_infinity());
  	s_d[k] = trunc_normal_rng(mu_sd,sig_sd,0,1);
  	s_r[k] = trunc_normal_rng(mu_sr,sig_sr,0,1);
    h_r[k] = exp(logh_r[k]);
    logw1[k] = -s_r[k]*pow(x1,c[k]).*log1p(h_r[k]*o1);
    logw2[k] = -s_r[k]*pow(x2,c[k]).*log1p(h_r[k]*o2);
  }
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  h_d = exp(logh_d);
  logd1 = -rep_matrix(s_d,nTrial).*log1p(h_d*t1);
  logd2 = -rep_matrix(s_d,nTrial).*log1p(h_d*t2);
  U1 = exp(logv1+logw1+logd1);
  U2 = exp(logv2+logw2+logd2);
  theta_logit = to_array_2d((rep_matrix(s,nTrial).*(U1-U2))');
  for(j in 1:nTrial){
    ypred[j] = bernoulli_logit_rng(theta_logit[j]);
  }
}
