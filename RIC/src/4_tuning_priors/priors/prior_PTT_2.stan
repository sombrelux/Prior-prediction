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
  vector<lower=0>[nTrial] x1;
  vector<lower=0>[nTrial] x2;
  vector<lower=0>[nTrial] t1;
  vector<lower=0>[nTrial] t2;
  vector<lower=0>[nTrial] p1;
  vector<lower=0>[nTrial] p2;
}
generated quantities{
  vector<lower=10^(-5)>[nPart] alpha;
  vector<upper=1>[nPart] beta;
  vector<lower=0>[nPart] gamma;
  vector<lower=0>[nPart] R;
  vector<lower=0>[nPart] s;
  
  vector<lower=0>[nTrial] v1[nPart];
  vector<lower=0>[nTrial] v2[nPart];
  vector<lower=0,upper=1>[nTrial] w1[nPart];
  vector<lower=0,upper=1>[nTrial] w2[nPart];
  vector<lower=0>[nTrial] U1[nPart];
  vector<lower=0>[nTrial] U2[nPart];
  real theta_logit[nPart,nTrial];
  int<lower=0,upper=1> ypred[nPart,nTrial];
  
  for(k in 1:nPart){
    alpha[k] = trunc_normal_rng(0.2,0.01,10^(-5),positive_infinity());
    beta[k] = trunc_normal_rng(0.59,0.02,negative_infinity(),1);
    gamma[k] = trunc_normal_rng(0.56,0.03,0,1);
    R[k] = trunc_normal_rng(10.5,1,0,positive_infinity());
    s[k] = trunc_normal_rng(2.8,0.2,0,positive_infinity());
  
    v1[k] =(1-exp(-alpha[k]*pow(x1,1-beta[k])))/alpha[k];
    v2[k] =(1-exp(-alpha[k]*pow(x2,1-beta[k])))/alpha[k];
    
    w1[k] = exp(-pow(R[k]*t1./x1-log(p1),gamma[k]));
    w2[k] = exp(-pow(R[k]*t2./x2-log(p2),gamma[k]));
  
    U1[k] = v1[k].*w1[k];
    U2[k] = v2[k].*w2[k];
  
    theta_logit[k] = to_array_1d(s[k]*(U1[k]-U2[k]));
  
    for(i in 1:nTrial){
      ypred[k,i] = bernoulli_logit_rng(theta_logit[k,i]);
    }
  }
}
