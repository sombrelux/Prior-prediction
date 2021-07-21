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
  real<lower=0,upper=100> a;
  real<lower=0,upper=100> s;
  real<lower=0,upper=10> c;
  real inv_hr;
  real<lower=0,upper=10> s_r;
  real inv_hd;
  real<lower=0,upper=10> s_d;
}
transformed parameters{
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector[nTrial] logw1;
  vector[nTrial] logw2;
  vector[nTrial] logd1;
  vector[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  
  logv1 = a*log(x1);
  logv2 = a*log(x2);
  
  logw1 = 
  
  for(j in 1:nTrial){
      //subjective values
      logv1[j] = a*logx1[j];
      logv2[j] = a*logx2[j];
      
      //uncertainty discounting
      logw1[j] = s_r*(log(inv_hr+o1[j])-log(inv_hr))*exp(c*logx1[j]);
      logw2[j] = s_r*(log(inv_hr+o2[j])-log(inv_hr))*exp(c*logx2[j]);
      
      //utilities
      U1[j] = exp(logv1[j]-logw1[j]);
      U2[j] = exp(logv2[j]-logw2[j]);
      
      theta_logit[j] = fmin(fmax(s*(U1[j]-U2[j]),-7),7);
      //print(theta_logit[j]);
  }
}
model{
  int grainsize=1;
  //priors

  a ~ normal(1,0.5);
  s ~ normal(0,1);
  c ~ normal(0,1);
  inv_hr ~ normal(1,2);
  s_r ~ normal(0,1);

  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta_logit,nPart);
}
generated quantities{
  int<lower=0> kpred[nTrial];

  for(i in 1:nTrial){
    kpred[i] = binomial_rng(nPart,inv_logit(theta_logit[i]));
  }
}
