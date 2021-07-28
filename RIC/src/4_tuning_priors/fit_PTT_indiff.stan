functions{
  real partial_sum(int[] k_slice, int start, int end, real[] theta) {
    real norm_temp=0.0;
    int  len = end-start+1;
    for(j in 1:len){
      norm_temp += normal_lpdf(k_slice[j]|theta[start+j-1],1);
    }
    return norm_temp;
  }
}
data{
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] p2;
  int<lower=0,upper=1> k[nTrial];
}
parameters{
  //group parameters
  real<lower=10^(-5)> alpha;
  real<lower=0,upper=1> beta;
  real<lower=0,upper=1> gamma;
  real<lower=0> R;
  real<lower=0> S;
}
transformed parameters{
  vector<lower=0,upper=1>[nTrial] v1;
  vector<lower=0,upper=1>[nTrial] v2;
  vector<lower=0,upper=1>[nTrial] w2;
  vector<lower=0,upper=1>[nTrial] U2;
  real<lower=0,upper=1> theta[nTrial];

  v1 =1-exp(-alpha*pow(x1,1-beta));
  v2 =1-exp(-alpha*pow(x2,1-beta));
  w2 = exp(-pow(R*t2./x2-log(p2),gamma));
  
  U2 = v2.*w2;

  theta = to_array_1d(inv_logit(s*(v1-U2)));
}
model{
  int grainsize=1;
  //priors

  alpha ~ normal(0,0.5);
  beta ~ beta(1,3);
  gamma ~ beta(3,1);
  R ~ normal(1,1);
  S ~ normal(8,2);

  //likelihood
  target += reduce_sum(partial_sum,k,grainsize,theta);
}
