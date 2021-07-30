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
  vector<lower=0,upper=1>[nPart] a;
  vector<lower=0,upper=1>[nPart] c;
  vector<lower=0>[nPart] loghr;
  vector<upper=0>[nPart] loghd;
  vector<lower=0,upper=1>[nPart] s_r;
  vector<lower=0,upper=1>[nPart] s_d;
  vector<lower=0>[nPart] s;
  
  vector<lower=0>[nPart] hr;
  vector<lower=0,upper=1>[nPart] hd;
  vector[nTrial] logv1[nPart];
  vector[nTrial] logv2[nPart];
  vector<upper=0>[nTrial] logw1[nPart];
  vector<upper=0>[nTrial] logw2[nPart];
  vector<upper=0>[nTrial] logd1[nPart];
  vector<upper=0>[nTrial] logd2[nPart];
  vector<lower=0>[nTrial] U1[nPart];
  vector<lower=0>[nTrial] U2[nPart];
  real theta_logit[nPart,nTrial];
  int<lower=0,upper=1> ypred[nPart,nTrial];
  
  for(k in 1:nPart){
    a[k] = trunc_normal_rng(0.4,0.2,0,1);
    c[k] = trunc_normal_rng(0.1,0.1,0,1);
    loghr[k] = trunc_normal_rng(0,1,0,positive_infinity());
    loghd[k] = trunc_normal_rng(-2,1,negative_infinity(),0);
    s_r[k] = trunc_normal_rng(0.6,0.4,0,1);
    s_d[k] = trunc_normal_rng(0.2,0.2,0,1);
    s[k] = trunc_normal_rng(0.2,0.3,0,positive_infinity());
    
  	hr[k] = exp(loghr[k]);
  	hd[k] = exp(loghd[k]);
    logv1[k] = a[k]*log(x1);
    logv2[k] = a[k]*log(x2);
    
    logw1[k] = -s_r[k]*pow(x1,c[k]).*log1p(hr[k]*o1);
    logw2[k] = -s_r[k]*pow(x2,c[k]).*log1p(hr[k]*o2);
    
    logd1[k] = -s_d[k]*log1p(hd[k]*t1);
    logd2[k] = -s_d[k]*log1p(hd[k]*t2);
  
    U1[k] = exp(logv1[k]+logd1[k]+logw1[k]);
    U2[k] = exp(logv2[k]+logd2[k]+logw2[k]);
    
    theta_logit[k] = to_array_1d(s[k]*(U1[k]-U2[k]));
    
    for(j in 1:nTrial){
      ypred[k,j] = bernoulli_logit_rng(theta_logit[k,j]);
    }
  }
}
