functions{
  real von_mises_pdf(real resp, real targ, real kappa){
    real vm_pdf;
    vm_pdf = exp(kappa*cos(resp-targ))/modified_bessel_first_kind(0,kappa);
  return vm_pdf;
  }
  real trunc_normal_rng(real mu, real sigma, real lb) {
    real p_lb = normal_cdf(lb,mu,sigma);
    real u = uniform_rng(p_lb,1);
    real y = mu + sigma * inv_Phi(u);
    return y;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> M; //max set size
  int<lower=1> N; //360 potential answer
  int<lower=1,upper=M> Setsize[nTrial];
  vector<lower=0,upper=2*pi()>[N] X; //possible responses
  
  vector<lower=0,upper=pi()>[M] D[nTrial]; //distance of location feature, the first element is the location of the target
  vector<lower=0,upper=2*pi()>[M] m[nTrial]; //color of stimulus, the first element is the color of the target
}
generated quantities{
  //individual parameters
  real<lower=0,upper=1> a;
  real<lower=0,upper=1> b;
  real<lower=0,upper=1> r;
  real<lower=0> s;
  real<lower=0,upper=18> kappa;
  real<lower=18> kappaf;
  
  //transformed parameters
  simplex[N] theta[nTrial];
  vector<lower=0>[N] Aa[nTrial]; //activation of features independent from location
  vector<lower=0>[N] Ab[nTrial]; //background noise
  vector<lower=0>[N] Ac[nTrial]; //activation associated with location feature
  vector<lower=0>[N] Af[nTrial]; //additional activation of attention
  vector<lower=0>[N] Ax[nTrial]; //total activation
  real<lower=0> vm_temp; //unnormalized vm_pdf
  real<lower=0, upper=1> p; //prob of focus at target
  real<lower=0, upper=1> q;
  
  //prior predictions
  real<lower=1,upper=360> ypred[nTrial];
  
  //individual parameters
  // reduce a,b, kappaf from prior_5
  a = beta_rng(1,5);
  b = beta_rng(2,10);
  r = beta_rng(1,5);
  s = trunc_normal_rng(12,10,0);
  kappa = trunc_normal_rng(12,2,0);
  kappaf = trunc_normal_rng(30,4,18);
  
  //transformed parameters
  for(j in 1:nTrial){
    p = 1.0/Setsize[j];
    q = 1-p;
    for(k in 1:N){
      Aa[j,k] = 0.0;
      Ac[j,k] = 0.0;
      
	  for(l in 1:Setsize[j]){
	      vm_temp = von_mises_pdf(X[k],m[j,l],kappa);
        Ac[j,k] += exp(-s*D[j,l])*vm_temp;
        Aa[j,k] += vm_temp;
      }
      Af[j,k] = von_mises_pdf(X[k],m[j,1],kappaf);
    }
	  Ab[j] = rep_vector(Setsize[j]*1.0,N);
	  Ax[j] = Ac[j]+p*Af[j]+(a*Aa[j]+b*Ab[j])*(q+p*r);
    theta[j] = Ax[j]/sum(Ax[j]);
    ypred[j] = categorical_rng(theta[j]);
  }
}
