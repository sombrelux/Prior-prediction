data{
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
}
generated quantities{
  real<lower=0> a;
  real<lower=0> h;
  real<lower=0> i;
  real<lower=0> s;
  vector<lower=0>[nTrial] v1;
  vector<lower=0>[nTrial] v2;
  vector<lower=1>[nTrial] invw1;
  vector<lower=1>[nTrial] invw2;
  vector<lower=0>[nTrial] U1;
  vector<lower=0>[nTrial] U2;
  real theta_logit[nTrial];
  int<lower=0,upper=1>[nTrial] ypred;
  
  a = normal_rng(1,1);
  h = normal_rng(1,1);
  i = normal_rng(1,1);
  s = normal_rng(1,1);
  
  v1 = pow(x1,a);
  v2 = pow(x2,a);
  
  invw1 = 1+h*(t1+i*o1);
  invw2 = 1+h*(t2+i*o2);
      
  U1 = v1./invw1;
  U2 = v2./invw2;
      
  theta_logit = to_array_1d(s*(U1-U2));
  
  for(i in 1:nTrial){
    ypred[i] = bernoulli_logit_rng(theta_logit[i]);
  }
}
