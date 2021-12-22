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
  row_vector<lower=0,upper=1>[nTrial] p1;
  row_vector<lower=0>[nTrial] t1;
  row_vector<lower=0>[nTrial] x2;
  row_vector<lower=0,upper=1>[nTrial] p2;
  row_vector<lower=0>[nTrial] t2;
  
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
  vector<lower=10^(-5),upper=1>[nPart] alpha;
  vector<lower=0,upper=1>[nPart] beta;
  vector<lower=0,upper=1>[nPart] gamma;
  vector<lower=0>[nPart] R;
  vector<lower=0>[nPart] s;
  matrix<lower=0>[nPart,nTrial] v1;
  matrix<lower=0>[nPart,nTrial] v2;
  matrix<lower=0,upper=1>[nPart,nTrial] w1;
  matrix<lower=0,upper=1>[nPart,nTrial] w2;
  matrix<lower=0>[nPart,nTrial] U1;
  matrix<lower=0>[nPart,nTrial] U2;
  array[nTrial,nPart] real theta_logit;
  array[nTrial,nPart] int<lower=0,upper=1> ypred;
  
  for(k in 1:nPart){
    alpha[k] = trunc_normal_rng(mu_alpha,sig_alpha,10^(-5),1);
  	beta[k] = trunc_normal_rng(mu_beta,sig_beta,0,1);
    gamma[k] = trunc_normal_rng(mu_gamma,sig_gamma,0,1);
    R[k] = trunc_normal_rng(mu_R,sig_R,0,positive_infinity());
  	s[k] = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());

    v1[k] = (1-exp(-alpha[k]*pow(x1,1-beta[k])))/alpha[k];
    v2[k] = (1-exp(-alpha[k]*pow(x2,1-beta[k])))/alpha[k];
    w1[k] = exp(-pow(R[k]*t1./x1-log(p1),gamma[k]));
    w2[k] = exp(-pow(R[k]*t2./x2-log(p2),gamma[k]));
  }

  U1 = v1.*w1;
  U2 = v2.*w2;
  theta_logit = to_array_2d((rep_matrix(s,nTrial).*(U1-U2))');
  for(j in 1:nTrial){
    ypred[j] = bernoulli_logit_rng(theta_logit[j]);
  }
}
