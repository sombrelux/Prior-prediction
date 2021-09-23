functions{
  vector von_mises_pdf(real resp, vector targ, real kappa){
    int n = num_elements(targ);
    vector[n] vm_pdf;
    vm_pdf = exp(kappa*cos(resp-targ))/modified_bessel_first_kind(0,kappa);
    return vm_pdf;
  }
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
  int<lower=1> nTrial;
  int<lower=1> N; //360 potential answer
  int<lower=1> Condition[nTrial]; //condition of cue types, 1 = both cues, 2 = color cue, 3 = location cue
  int<lower=1> M; //set size=6 in Exp4
  vector<lower=0,upper=2*pi()>[N] X; //360 possible responses
  row_vector<lower=0,upper=pi()>[M] Dloc[nTrial]; //distance of location, the first element is the location of the target
  row_vector<lower=0,upper=pi()>[M] Dcol[nTrial]; //distance of color, the first element is the color of the target
  vector<lower=0,upper=2*pi()>[M] m[nTrial]; //orientations, the first element is the orientation of the target
}
generated quantities{
  //group parameters
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0,upper=1> r;
  real<lower=0> sloc;
  real<lower=0> scol;
  real<lower=0> kappa;
  real<lower=0> delta;
  real<lower=0,upper=1> w;
  
  //transformed parameters
  real<lower=0> kappaf;
  simplex[N] theta[nTrial];
  vector<lower=0>[N] Aa[nTrial]; //activation of features independent from location
  vector<lower=0>[N] Ab = rep_vector(M*1.0,N);; //background noise
  vector<lower=0>[N] Ac[nTrial]; //activation associated with location feature
  vector<lower=0>[N] Af[nTrial]; //additional activation of attention
  vector<lower=0>[N] Ax[nTrial]; //total activation
  
  real<lower=0, upper=1> q; //common coefficient of Aa and Ab
  real<lower=0,upper=1> W; //weight transformed according to condition
  vector<lower=0>[M] vm_temp; //unnormalized vm_pdf

  //prior predictions
  real<lower=1,upper=N> ypred[nTrial];
  
  //informative priors
  a = trunc_normal_rng(0,1,0,positive_infinity());
  b = trunc_normal_rng(0,1,0,positive_infinity());
  r = trunc_normal_rng(0,1,0,1);
  sloc = trunc_normal_rng(5,100,0,positive_infinity());
  scol = trunc_normal_rng(5,100,0,positive_infinity());
  kappa = trunc_normal_rng(7,100,0,positive_infinity());
  delta = trunc_normal_rng(20,100,0,positive_infinity());
  w = trunc_normal_rng(1,1,0,1);
  
  //transformed parameters
  kappaf = kappa+delta;
  q = 1+(r-1)/M;
  
  //likelihood
  for(j in 1:nTrial){
  	if(Condition[j]==3) //loc cond 
  	  W = 1;
  	else if(Condition[j]==2) //col cond
  	  W = 0;
  	else
  	  W = w; //both cond
    for(k in 1:N){
	    vm_temp = von_mises_pdf(X[k],m[j],kappa);
	    Ac[j,k] = (W*exp(-sloc*Dloc[j])+(1-W)*exp(-scol*Dcol[j]))*vm_temp;
      Aa[j,k] = sum(vm_temp);
      Af[j,k] = exp(kappaf*cos(X[k]-m[j,1]))/modified_bessel_first_kind(0,kappaf);
    }
	  Ax[j] = Ac[j]+Af[j]/M+(a*Aa[j]+b*Ab)*q;
    theta[j] = Ax[j]/sum(Ax[j]);
	  ypred[j] = categorical_rng(theta[j]);
  }
}