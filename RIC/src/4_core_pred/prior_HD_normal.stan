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
  real<lower=0> mu_i;
  real mu_logh;
  real<lower=0> mu_s;
  real<lower=0> sig_a;
  real<lower=0> sig_i;
  real<lower=0> sig_logh;
  real<lower=0> sig_s;
}
generated quantities{
  vector<lower=0,upper=2>[nPart] a;
  vector[nPart] logh;
  vector<lower=0>[nPart] i;
  vector<lower=0>[nPart] s;
  vector<lower=0>[nPart] h;
  matrix<lower=0>[nPart,nTrial] v1;
  matrix<lower=0>[nPart,nTrial] v2;
  matrix<lower=1>[nPart,nTrial] invw1;
  matrix<lower=1>[nPart,nTrial] invw2;
  matrix<lower=0>[nPart,nTrial] U1;
  matrix<lower=0>[nPart,nTrial] U2;
  array[nTrial,nPart] real theta_logit;
  array[nTrial,nPart] int<lower=0,upper=1> ypred;
  
  for(k in 1:nPart){
    a[k] = trunc_normal_rng(mu_a,sig_a,0,2);
    i[k] = trunc_normal_rng(mu_i,sig_i,0,positive_infinity());
    logh[k] = normal_rng(mu_logh,sig_logh);
    s[k] = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());
	  v1[k] = pow(x1,a[k]);
	  v2[k] = pow(x2,a[k]);
  }
  h = exp(logh);
  invw1 = 1+h*t1+(h.*i)*o1;
  invw2 = 1+h*t2+(h.*i)*o2;
  U1 = v1./invw1;
  U2 = v2./invw2;
  theta_logit = to_array_2d((rep_matrix(s,nTrial).*(U1-U2))');
  for(j in 1:nTrial){
    ypred[j] = bernoulli_logit_rng(theta_logit[j]);
  }
}
