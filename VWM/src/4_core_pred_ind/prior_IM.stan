functions{
  vector von_mises_pdf(real resp, vector targ, real kappa){
    int n = num_elements(targ);
    vector[n] vm_pdf;
    vm_pdf = exp(kappa*cos(resp-targ))/modified_bessel_first_kind(0,kappa);
    return vm_pdf;
  }
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
  array[nTrial] int<lower=1,upper=nPart> ID;
  int<lower=1> N; //360 potential answer
  int<lower=1> Condition[nTrial]; //condition of cue types, 1 = both cues, 2 = color cue, 3 = location cue
  int<lower=1> M; //set size=6 in Exp4
  vector<lower=0,upper=2*pi()>[N] X; //360 possible responses
  row_vector<lower=0,upper=pi()>[M] Dloc[nTrial]; //distance of location, the first element is the location of the target
  row_vector<lower=0,upper=pi()>[M] Dcol[nTrial]; //distance of color, the first element is the color of the target
  vector<lower=0,upper=2*pi()>[M] m[nTrial]; //orientations, the first element is the orientation of the target
  
  real<lower=0> mu_a;
  real<lower=0> mu_b;
  real<lower=0,upper=1> mu_r;
  //real<lower=0> mu_sloc;
  real<lower=0> mu_s;
  real<lower=0> mu_kappa;
  real<lower=0> mu_delta;
  
  real<lower=0> sig_a;
  real<lower=0> sig_b;
  real<lower=0> sig_r;
  //real<lower=0> sig_sloc;
  real<lower=0> sig_s;
  real<lower=0> sig_kappa;
  real<lower=0> sig_delta;
  
  real<lower=0> a_w;
  real<lower=0> b_w;
  
}
generated quantities{
  //group parameters
  vector<lower=0>[nPart] a;
  vector<lower=0>[nPart] b;
  vector<lower=0,upper=1>[nPart] r;
  vector<lower=0>[nPart] kappa;
  vector<lower=0>[nPart] delta;
  vector<lower=0>[nPart] sloc;
  vector<lower=0>[nPart] scol;
  vector<lower=0,upper=1>[nPart] w;
  
  //transformed parameters
  vector<lower=0>[nPart] kappaf;
  simplex[N] theta[nTrial];
  vector<lower=0>[N] Ab = rep_vector(M*1.0,N); //background noise
  array[nTrial] vector<lower=0>[N] Aa; //activation of features independent from location
  array[nTrial] vector<lower=0>[N] Ac; //activation associated with location feature
  array[nTrial] vector<lower=0>[N]  Af; //additional activation of attention
  array[nTrial] vector<lower=0>[N]  Ax; //total activation
  
  vector<lower=0, upper=1>[nPart] q; //common coefficient of Aa and Ab
  real<lower=0,upper=1> W; //weight transformed according to condition
  vector<lower=0>[M] vm_temp; //unnormalized vm_pdf

  //prior predictions
  array[nTrial] int<lower=1,upper=N> ypred;
  
  //informative priors
  for(k in 1:nPart){
    a[k] = trunc_normal_rng(mu_a,sig_a,0,positive_infinity());
	b[k] = trunc_normal_rng(mu_b,sig_b,0,positive_infinity());
	r[k] = trunc_normal_rng(mu_r,sig_r,0,1);
	kappa[k] = trunc_normal_rng(mu_kappa,sig_kappa,0,positive_infinity());
	delta[k] = trunc_normal_rng(mu_delta,sig_delta,0,positive_infinity());
	sloc[k] = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());
	scol[k] = trunc_normal_rng(mu_s,sig_s,0,positive_infinity());
	w[k] = beta_rng(a_w,b_w);
  }
  
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
  	  W = w[ID[j]]; //both cond
    for(i in 1:N){
      vm_temp = von_mises_pdf(X[i],m[j],kappa[ID[j]]);
	  Ac[j,i] = (W*exp(-sloc[ID[j]]*Dloc[j])+(1-W)*exp(-scol[ID[j]]*Dcol[j]))*vm_temp;
      Aa[j,i] = sum(vm_temp);
      Af[j,i] = exp(kappaf[ID[j]]*cos(X[i]-m[j,1]))/modified_bessel_first_kind(0,kappaf[ID[j]]);
    }
    Ax[j] = Ac[j]+Af[j]/M+(a[ID[j]]*Aa[j]+b[ID[j]]*Ab)*q[ID[j]];
    theta[j] = Ax[j]/sum(Ax[j]);
    ypred[j] = categorical_rng(theta[j]);
  }
}
