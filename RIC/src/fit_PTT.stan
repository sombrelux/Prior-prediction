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
  vector<lower=0,upper=1>[nTrial] p1;
  vector<lower=0,upper=1>[nTrial] p2;
  int<lower=0,upper=N> k[nTrial];
}
parameters{
  //group parameters
  real<lower=10^(-5)> alpha;
  real<upper=1> beta;
  real<lower=0,upper=1> gamma;
  real<lower=0> R;
  real<lower=0> s;
}
transformed parameters{
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=0,upper=1>[nTrial] w1;
  vector<lower=0,upper=1>[nTrial] w2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];

  v1 =(1-exp(-alpha*pow(x1,1-beta)))/alpha;
  v2 =(1-exp(-alpha*pow(x2,1-beta)))/alpha;
  
  w1 = exp(-pow(R*t1./x1-log(p1),gamma));
  w2 = exp(-pow(R*t2./x2-log(p2),gamma));
  
  U1 = v1.*w1;
  U2 = v2.*w2;

  theta_logit = to_array_1d(s*(U1-U2));
}
model{
  int grainsize=1;
  //priors

  alpha ~ normal(1,1);
  beta ~ normal(1,1);
  gamma ~ beta(1,1);
  R ~ normal(1,1);
  s ~ normal(1,1);

  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta_logit,n);
}