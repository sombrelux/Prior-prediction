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
  int<lower=1> nExp;
  int<lower=1> nTrial;
  array[nTrial] int<lower=1> Exp;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
  array[nTrial] int<lower=1> N;
  array[nTrial] int<lower=0> y;
}
parameters{
  //group parameters
  real<lower=0,upper=2> a;
  real<lower=0,upper=1> c;
  real logh_d;
  real logh_r;
  real<lower=0> s_d;
  real<lower=0> s_r;
  real<lower=0> s;
  real<lower=0> sig;
  
  vector<lower=0,upper=2>[nExp] a_i;
  vector<lower=0,upper=1>[nExp] c_i;
  vector[nExp] loghd_i;
  vector[nExp] loghr_i;
  vector<lower=0>[nExp] sd_i;
  vector<lower=0>[nExp] sr_i;
  vector<lower=0>[nExp] s_i;
}
transformed parameters{
  vector<lower=0>[nExp] hd_i=exp(loghd_i);
  vector<lower=0>[nExp] hr_i=exp(loghr_i);
  vector[nTrial] logv1;
  vector[nTrial] logv2;
  vector[nTrial] logw1;
  vector[nTrial] logw2;
  vector<upper=0>[nTrial] logd1;
  vector<upper=0>[nTrial] logd2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  array[nTrial] real theta_logit;
  
  for(j in 1:nTrial){
    logv1[j] = a_i[Exp[j]]*log(x1[j]);
    logv2[j] = a_i[Exp[j]]*log(x2[j]);
    logw1[j] = -sr_i[Exp[j]]*pow(x1[j],c_i[Exp[j]]).*log1p(hr_i[Exp[j]]*o1[j]);
    logw2[j] = -sr_i[Exp[j]]*pow(x2[j],c_i[Exp[j]]).*log1p(hr_i[Exp[j]]*o2[j]);
    logd1[j] = -sd_i[Exp[j]]*log1p(hd_i[Exp[j]]*t1[j]);
    logd2[j] = -sd_i[Exp[j]]*log1p(hd_i[Exp[j]]*t2[j]);
    
    U1[j] = exp(logv1[j]+logw1[j]+logd1[j]);
    U2[j] = exp(logv2[j]+logw2[j]+logd2[j]);
    theta_logit[j] = fmin(fmax(s_i[Exp[j]]*(U1[j]-U2[j]),-10),10);
  }
}
model{
  int grainsize=1;
  //group priors
  a ~ normal(0,1)T[0,2];
  c ~ normal(0,1)T[0,1];
  logh_r ~ normal(1,1);
  logh_d ~ normal(-1,1);
  s_r ~ normal(0,1);
  s_d ~ normal(0,1);
  s ~ normal(0,1)T[0,];
  sig ~ normal(0,1)T[0,];
  //individ priors
  for(i in 1:nExp){
      a_i[i] ~ normal(a,sig)T[0,2];
      c_i[i] ~ normal(c,sig)T[0,1];
      loghr_i[i] ~ normal(logh_r,sig);
      loghd_i[i] ~ normal(logh_d,sig);
      sr_i[i] ~ normal(s_r,sig);
      sd_i[i] ~ normal(s_d,sig);
      s_i[i] ~ normal(s,sig)T[0,];
  }

  
  //likelihood
  target += reduce_sum(partial_sum,y,grainsize,theta_logit,N);
}
generated quantities{
  array[nTrial] int<lower=0> ypred;
  real<lower=0,upper=1> theta[nTrial];
  for(j in 1:nTrial){
    theta[j]  = inv_logit(theta_logit[j]);
    ypred[j] = binomial_rng(N[j],theta[j]);
  }
}
