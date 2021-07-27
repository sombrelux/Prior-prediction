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
  real<lower=0,upper=1> c;
  real loghr;
  real<upper=0> loghd;
  real<lower=0,upper=1> s_r;
  real<lower=0> s_d;
  real<lower=0> s;
}
transformed parameters{
  real<lower=0> hr = exp(loghr);
  real<lower=0> hd = exp(loghd);
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector<upper=0>[nTrial] logw1;
  vector<upper=0>[nTrial] logw2;
  vector<upper=0>[nTrial] logd1;
  vector<upper=0>[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  
  logw1 = -s_r*pow(x1,c).*log1p(hr*o1);
  logw2 = -s_r*pow(x2,c).*log1p(hr*o2);
  
  logd1 = -s_d*log1p(hd*t1);
  logd2 = -s_d*log1p(hd*t2);
  
  U1 = exp(logv1+logd1+logw1);
  U2 = exp(logv2+logd2+logw2);
  
  theta_logit = to_array_1d(s*(U1-U2));
}
model{
  int grainsize=1;
  //priors
  a ~ beta(1,1);
  c ~ normal(0,1);
  loghr ~ normal(1.5,1);
  loghd ~ normal(-1.5,1);
  s_r ~ beta(1,1);
  s_d ~ normal(1,1);
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
