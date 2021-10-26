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
}
generated quantities{
  vector<lower=0>[nPart] a;
  vector[nPart] logh;
  vector<lower=0>[nPart] i;
  vector[nPart] logs;
  vector<lower=0>[nPart] h;
  vector<lower=0>[nPart] s;
  matrix<lower=0>[nPart,nTrial] v1;
  matrix<lower=0>[nPart,nTrial] v2;
  matrix<lower=1>[nPart,nTrial] invw1;
  matrix<lower=1>[nPart,nTrial] invw2;
  matrix<lower=0>[nPart,nTrial] U1;
  matrix<lower=0>[nPart,nTrial] U2;
  array[nTrial,nPart] real theta_logit;
  array[nTrial,nPart] int<lower=0,upper=1> ypred;
  
  for(k in 1:nPart){
    a[k] = trunc_normal_rng(0.25,1,0,positive_infinity());
    i[k] = trunc_normal_rng(1.1,1,0,positive_infinity());
    logh[k] = normal_rng(-2.1,1);
    logs[k] = normal_rng(0.45,1);
	  v1[k] = pow(x1,a[k]);
	  v2[k] = pow(x2,a[k]);
  }
  h = exp(logh);
  s = exp(logs);
  invw1 = 1+h*t1+(h.*i)*o1;
  invw2 = 1+h*t2+(h.*i)*o2;
  U1 = v1./invw1;
  U2 = v2./invw2;
  theta_logit = to_array_2d((rep_matrix(s,nTrial).*(U1-U2))');
  for(j in 1:nTrial){
    ypred[j] = bernoulli_logit_rng(theta_logit[j]);
  }
}
