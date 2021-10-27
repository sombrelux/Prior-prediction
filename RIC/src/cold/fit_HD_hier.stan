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
  int<lower=1> Exp[nTrial];
  int<lower=1> N[nTrial];
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
  int<lower=0> y[nTrial];
}
parameters{
  //group parameters
  real<lower=0,upper=1> a;
  real logh;
  real<lower=0> i;
  real<lower=0> s;  
  real<lower=0> sd_a;
  real<lower=0> sd_logh;
  real<lower=0> sd_i;
  real<lower=0> sd_s;
  
  vector<lower=0,upper=1>[nExp] a_i;
  vector<lower=0>[nExp] logh_i;
  vector<lower=0>[nExp] i_i;
  vector<lower=0>[nExp] s_i;
}
transformed parameters{
  vector<lower=0>[nExp] h_i = exp(logh_i);
  vector<lower=0>[nExp] SD_a = rep_vector(sd_a,nExp);
  vector<lower=0>[nExp] SD_logh = rep_vector(sd_logh,nExp);
  vector<lower=0>[nExp] SD_i = rep_vector(sd_i,nExp);
  vector<lower=0>[nExp] SD_s = rep_vector(sd_s,nExp);
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=1>[nTrial] invw1;
  vector<lower=1>[nTrial] invw2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  for(j in 1:nTrial){
    v1[j] = pow(x1[j],a_i[Exp[j]]);
    v2[j] = pow(x2[j],a_i[Exp[j]]);
    invw1[j] = 1+h_i[Exp[j]]*(t1[j]+i_i[Exp[j]]*o1[j]);
    invw2[j] = 1+h_i[Exp[j]]*(t2[j]+i_i[Exp[j]]*o2[j]);
    U1[j] = v1[j]/invw1[j];
    U2[j] = v2[j]/invw2[j];
    theta_logit[j] = s_i[Exp[j]]*(U1[j]-U2[j]);
  }
}
model{
  int grainsize=1;
  //priors
  a ~ beta(1,1);
  logh ~ normal(0,1);
  i ~ normal(35,10);
  s ~ normal(0,10);
  sd_a ~ cauchy(0,1);
  sd_logh ~ cauchy(0,1);
  sd_i ~ cauchy(0,1);
  sd_s ~ cauchy(0,1);
  
  a_i ~ normal(a,SD_a);
  logh_i ~ normal(logh,SD_logh);
  i_i ~ normal(i,SD_i);
  s_i ~ normal(s,SD_s);
  
  //likelihood
  target += reduce_sum(partial_sum,y,grainsize,theta_logit,N);
}
generated quantities{
  vector[nTrial] ypred;
  real<lower=0,upper=1> theta[nTrial];
  for(j in 1:nTrial){
    theta[j]  = inv_logit(theta_logit[j]);
    ypred[j] = binomial_rng(N[j],theta[j]);
  }
}
