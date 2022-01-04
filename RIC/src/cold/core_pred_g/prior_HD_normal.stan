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
  real<lower=0> mu_i;
  real mu_logh;
  real<lower=0> mu_s;
  real<lower=0> sig_a;
  real<lower=0> sig_i;
  real<lower=0> sig_logh;
  real<lower=0> sig_s;
}
generated quantities{
  real<lower=0,upper=2> a;
  real<upper=0> logh;
  real<lower=0> i;
  real<lower=0> s;
  real<lower=0,upper=1> h;
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=1>[nTrial] invw1;
  vector<lower=1>[nTrial] invw2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  array[nTrial] real<lower=0,upper=1> theta;
  array[nTrial] int<lower=0,upper=nPart> ypred;
  
  a = trunc_normal_rng(mu_a,sig_a,0,2);
  i = trunc_normal_rng(mu_i,sig_i,0,positive_infinity());
  logh = trunc_normal_rng(mu_logh,sig_logh,negative_infinity(),0);
  s = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());
  v1 = pow(x1,a);
  v2 = pow(x2,a);
  
  h = exp(logh);
  invw1 = 1+h*(t1+i*o1);
  invw2 = 1+h*(t2+i*o2);
  U1 = v1./invw1;
  U2 = v2./invw2;
  theta = to_array_1d(inv_logit(s*(U1-U2)));
  for(j in 1:nTrial){
    ypred[j] = binomial_rng(nPart,theta[j]);
  }
}
