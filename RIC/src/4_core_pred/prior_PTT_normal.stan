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
  vector<lower=0,upper=1>[nTrial] p1;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0,upper=1>[nTrial] p2;
  vector<lower=0>[nTrial] t2;
  
  real<lower=10^(-5)> mu_alpha;
  real<lower=0,upper=1> mu_beta;
  real<lower=0,upper=1> mu_gamma;
  real<lower=0> mu_R;
  real<lower=0> mu_s;
  real<lower=0> sig_alpha;
  real<lower=0> sig_beta;
  real<lower=0> sig_gamma;
  real<lower=0> sig_R;
  real<lower=0> sig_s;
}
generated quantities{
  real<lower=10^(-5),upper=1> alpha;
  real<lower=0,upper=1> beta;
  real<lower=0,upper=1> gamma;
  real<lower=0> R;
  real<lower=0> s;
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=0,upper=1>[nTrial] w1;
  vector<lower=0,upper=1>[nTrial] w2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  array[nTrial] real<lower=0,upper=1> theta;
  array[nTrial] int<lower=0,upper=nPart> ypred;
  
  alpha = trunc_normal_rng(mu_alpha,sig_alpha,10^(-5),1);
  beta = trunc_normal_rng(mu_beta,sig_beta,0,1);
  gamma = trunc_normal_rng(mu_gamma,sig_gamma,0,1);
  R = trunc_normal_rng(mu_R,sig_R,0,positive_infinity());
  s = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());

  v1 = (1-exp(-alpha*pow(x1,1-beta)))/alpha;
  v2 = (1-exp(-alpha*pow(x2,1-beta)))/alpha;
  w1 = exp(-pow(R*t1./x1-log(p1),gamma));
  w2 = exp(-pow(R*t2./x2-log(p2),gamma));

  U1 = v1.*w1;
  U2 = v2.*w2;
  theta = to_array_1d(inv_logit(s*(U1-U2)));
  for(j in 1:nTrial){
    ypred[j] = binomial_rng(nPart,theta[j]);
  }
}
