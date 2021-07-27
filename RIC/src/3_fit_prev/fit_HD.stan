functions{
  real partial_sum(int[] k_slice, int start, int end, real[] theta_logit,int[] n) {
    real binom_temp=0.0;
    int  len = end-start+1;
    for(j in 1:len){
      binom_temp += binomial_logit_lpmf(k_slice[j]|n[start+j-1],theta_logit[start+j-1]);
    }
    return binom_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> n[nTrial];
  int<lower=1> N;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
  int<lower=0,upper=N> k[nTrial];
}
parameters{
  //group parameters
  real<lower=0,upper=1> a;
  real logh;
  real<lower=0> i;
  real<lower=0> s;
}
transformed parameters{
  real<lower=0> h = exp(logh);
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=1>[nTrial] invw1;
  vector<lower=1>[nTrial] invw2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  
  v1 = pow(x1,a);
  v2 = pow(x2,a);
  
  invw1 = 1+h*(t1+i*o1);
  invw2 = 1+h*(t2+i*o2);
      
  U1 = v1./invw1;
  U2 = v2./invw2;
      
  theta_logit = to_array_1d(s*(U1-U2));
}
model{
  int grainsize=1;
  //priors

  a ~ beta(1,1);
  logh ~ normal(-7,1);
  i ~ normal(40,1);
  s ~ normal(1,1);

  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta_logit,n);
}
generated quantities{
	vector[nTrial] kpred;
	real<lower=0,upper=1> theta[nTrial];
	for(j in 1:nTrial){
	  theta[j]  = inv_logit(theta_logit[j]);
	  kpred[j] = binomial_rng(n[j],theta[j]);
	}
}
