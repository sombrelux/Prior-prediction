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
  int<lower=1> nPart;
  int<lower=1> nTrial;
  int<lower=1,upper=nPart> ID[nTrial]; // subj ID
  int<lower=1> N; //360 potential answer
  int<lower=1> Condition[nTrial]; //condition of cue types, 1 = both cues, 2 = color cue, 3 = location cue
  int<lower=1> M; //set size=6 in Exp4
  vector<lower=0,upper=2*pi()>[N] X; //360 possible responses
  row_vector<lower=0,upper=pi()>[M] Dloc[nTrial]; //distance of location, the first element is the location of the target
  row_vector<lower=0,upper=pi()>[M] Dcol[nTrial]; //distance of color, the first element is the color of the target
  vector<lower=0,upper=2*pi()>[M] m[nTrial]; //orientations, the first element is the orientation of the target
}
generated quantities{
  int<lower=1,upper=nPart> id;
  //group parameters
  real<lower=0> mu_a;
  real<lower=0> mu_b;
  real<lower=0,upper=1> mu_r;
  real<lower=0> mu_sloc;
  real<lower=0> mu_scol;
  real<lower=0> mu_kappa;
  real<lower=0> mu_delta;
  real<lower=0,upper=1> mu_w;
  //individual parameters
  vector<lower=0>[nPart] a;
  vector<lower=0>[nPart] b;
  vector<lower=0,upper=1>[nPart] r;
  vector<lower=0>[nPart] sloc;
  vector<lower=0>[nPart] scol;
  vector<lower=0>[nPart] kappa;
  vector<lower=0>[nPart] delta;
  vector<lower=0,upper=1>[nPart] w;  
  
  //transformed parameters
  vector<lower=0>[nPart] kappaf;
  simplex[N] theta[nTrial];
  vector<lower=0>[N] Aa[nTrial]; //activation of features independent from location
  vector<lower=0>[N] Ab = rep_vector(M*1.0,N);; //background noise
  vector<lower=0>[N] Ac[nTrial]; //activation associated with location feature
  vector<lower=0>[N] Af[nTrial]; //additional activation of attention
  vector<lower=0>[N] Ax[nTrial]; //total activation
  
  vector<lower=0, upper=1>[nPart] q; //common coefficient of Aa and Ab
  real<lower=0,upper=1> W; //weight transformed according to condition
  vector<lower=0>[M] vm_temp; //unnormalized vm_pdf

  //prior predictions
  real<lower=1,upper=N> ypred[nTrial];
  
  //informative priors
  mu_a = trunc_normal_rng(0,0.3,0,positive_infinity());
  mu_b = trunc_normal_rng(0,0.3,0,positive_infinity());
  mu_r = trunc_normal_rng(0,0.3,0,1);
  mu_sloc = trunc_normal_rng(5,10,0,positive_infinity());
  mu_scol = trunc_normal_rng(5,10,0,positive_infinity());
  mu_kappa = trunc_normal_rng(7,10,0,positive_infinity());
  mu_delta = trunc_normal_rng(20,10,0,positive_infinity());
  mu_w = trunc_normal_rng(1,0.3,0,1);
  
  for(i in 1:nPart){
    a[i] = trunc_normal_rng(mu_a,0.1,0,positive_infinity());
    b[i] = trunc_normal_rng(mu_b,0.1,0,positive_infinity());
    r[i] = trunc_normal_rng(mu_r,0.1,0,1);
    sloc[i] = trunc_normal_rng(mu_sloc,3,0,positive_infinity());
    scol[i] = trunc_normal_rng(mu_scol,3,0,positive_infinity());
    kappa[i] = trunc_normal_rng(mu_kappa,3,0,positive_infinity());
    delta[i] = trunc_normal_rng(mu_delta,3,0,positive_infinity());
    w[i] = trunc_normal_rng(mu_w,0.1,0,1);
  }
  
  //transformed parameters
  kappaf = kappa+delta;
  q = 1+(r-1)/M;
  
  //likelihood
  for(j in 1:nTrial){
	  id = ID[j]; //subjID
  	if(Condition[j]==3) //loc cond 
  	  W = 1;
  	else if(Condition[j]==2) //col cond
  	  W = 0;
  	else
  	  W = w[id]; //both cond
    for(k in 1:N){
	    vm_temp = von_mises_pdf(X[k],m[j],kappa[id]);
	    Ac[j,k] = (W*exp(-sloc[id]*Dloc[j])+(1-W)*exp(-scol[id]*Dcol[j]))*vm_temp;
      Aa[j,k] = sum(vm_temp);
      Af[j,k] = exp(kappaf[id]*cos(X[k]-m[j,1]))/modified_bessel_first_kind(0,kappaf[id]);
    }
	  Ax[j] = Ac[j]+Af[j]/M+(a[id]*Aa[j]+b[id]*Ab)*q[id];
    theta[j] = Ax[j]/sum(Ax[j]);
	  ypred[j] = categorical_rng(theta[j]);
  }
}
