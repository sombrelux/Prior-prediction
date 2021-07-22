functions{
  real trunc_normal_rng(real mu, real sigma, real lb, real ub) {
    real p_lb = normal_cdf(lb,mu,sigma);
    real p_ub = 1;
    real u;
    real y;
    if(!is_inf(ub)){
      p_ub = normal_cdf(ub,mu,sigma);
    }
    u = uniform_rng(p_lb,p_ub);
    y = mu + sigma * inv_Phi(u);
    return y;
  }
}
data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] o1;
  vector<lower=0>[nTrial] o2;
}
generated quantities{
  vector<lower=0>[nPart] a;
  vector<lower=0>[nPart] h;
  vector<lower=0>[nPart] i;
  vector<lower=0>[nPart] s;
  vector<lower=0>[nTrial] v1[nPart];
  vector<lower=0>[nTrial] v2[nPart];
  vector<lower=1>[nTrial] invw1[nPart];
  vector<lower=1>[nTrial] invw2[nPart];
  vector<lower=0>[nTrial] U1[nPart];
  vector<lower=0>[nTrial] U2[nPart];
  real theta_logit[nPart,nTrial];
  int<lower=0,upper=1> ypred[nP,nTrial];
  
  for(k in 1:nPart){
    a[k] = trunc_normal_rng(0.2,0.1,0,positive_infinity());
    h[k] = trunc_normal_rng(0.45,0.1,0,positive_infinity());
    i[k] = trunc_normal_rng(1,0.1,0,positive_infinity());
    s[k] = trunc_normal_rng(3,0.1,0,positive_infinity());
  
    v1[k] = pow(x1,a[k]);
    v2[k] = pow(x2,a[k]);
    
    invw1[k] = 1+h[k]*(t1+i[k]*o1);
    invw2[k] = 1+h[k]*(t2+i[k]*o2);
        
    U1[k] = v1[k]./invw1[k];
    U2[k] = v2[k]./invw2[k];
        
    theta_logit[k] = to_array_1d(s*(U1-U2));
  
    for(j in 1:nTrial){
      ypred[k,j] = bernoulli_logit_rng(theta_logit[k,j]);
    }
}
