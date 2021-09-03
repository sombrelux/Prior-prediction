functions{  
  real partial_sum(int[] x_slice, int start, int end, int M, int N, vector Condition,row_vector[] Dcol,row_vector[] Dloc, matrix[] E, real a, real b, real kappa, real kappaf,real sloc,real scol, real w, real q) {
    int len = end-start+1;
    int k;
    matrix[M, N] vm_pdf_temp;
    matrix[len,N] Ab = rep_matrix(M[start:end], N); //background noise
    matrix[len,N] Aa; //activation of features independent from location
    matrix[len,N] Ac; //activation associated with location feature
    matrix[len,N] Af; //additional activation of attention
    matrix[len,N] Ax; //total activation
    vector[N] theta[len];
    real ctg_temp = 0.0;
    
    for(j in 1:len){
      k = start+j-1;
	  if(Condition[k]==3) //loc cond 
  	    W = 1;
  	  else if(Condition[k]==2) //col cond
  	    W = 0;
  	  else
  	    W = w; //both cond
      vm_pdf_temp = exp(kappa*cos(E[k]))/modified_bessel_first_kind(0,kappa);
      Ac[j] = (W*exp(-sloc*Dloc[k])+(1-W)*exp(-scol*Dcol[k]))*vm_pdf_temp;
      Aa[j] = sum(vm_pdf_temp);
      Af[j] = exp(kappaf*cos(E[k,1]))/modified_bessel_first_kind(0,kappaf);
      Ax[j] = Ac[j]+Af[j]/M+(a*Aa[j]+b*Ab[j])*q;
      theta[j] = Ax[j]'/sum(Ax[j]);
	    ctg_temp += categorical_lpmf(x_slice[j]|theta[j]);
	  } 
    return ctg_temp;
  }
}
data{
  int<lower=1> nTrial;
  int<lower=1> N;
  int<lower=1> Condition[nTrial]; //condition of cue types, 1 = both cues, 2 = color cue, 3 = location cue
  int<lower=1> M; //set size=6 in Exp4
  row_vector<lower=0,upper=pi()>[M] Dloc[nTrial]; //distance of location, the first element is the location of the target
  row_vector<lower=0,upper=pi()>[M] Dcol[nTrial]; //distance of color, the first element is the color of the target
  matrix<lower=-pi(),upper=pi()>[M,N] E[nTrial]; //possible response errors in radian
  int<lower=1,upper=N> x[nTrial]; // responses in degree
}
parameters{
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0,upper=1> r;
  real<lower=0> sloc;
  real<lower=0> scol;
  real<lower=0> kappa;
  real<lower=0> delta;
  real<lower=0,upper=1> w;
}
transformed parameters{
  real<lower=0> kappaf = kappa+delta;
  real<lower=0,upper1> q = 1+(r-1)/M;
}
model{
  int grainsize = 1;
  a ~ normal(0,0.3);
  b ~ normal(0,0.3);
  r ~ normal(0,0.3);
  sloc ~ normal(5,10);
  scol ~ normal(5,10);
  kappa ~ normal(7,10);
  delta ~ normal(20,10);
  w ~ normal(1,0.3);
  //likelihood
  target += reduce_sum(partial_sum, x , grainsize, M, N, Condition,Dcol,Dloc, E, a, b,kappa, kappaf,sloc,scol,w,q);
}
generated quantities{
  int<lower=1,upper=N> xpred[nTrial];
  matrix<lower=0>[M, N] vm_pdf_temp;
  matrix<lower=0>[nTrial,N] Ab = rep_matrix(Setsize, N); //background noise
  matrix<lower=0>[nTrial,N] Aa; //activation of features independent from location
  matrix<lower=0>[nTrial,N] Ac; //activation associated with location feature
  matrix<lower=0>[nTrial,N] Af; //additional activation of attention
  matrix<lower=0>[nTrial,N] Ax; //total activation
  vector<lower=0,upper=1>[N] theta[nTrial];
  
  for(j in 1:nTrial){
  	if(Condition[j]==3) //loc cond 
  	  W = 1;
  	else if(Condition[j]==2) //col cond
  	  W = 0;
  	else
  	  W = w; //both cond
    vm_pdf_temp = exp(kappa*cos(E[j]))/modified_bessel_first_kind(0,kappa);
    Ac[j] = (W*exp(-sloc*Dloc[j])+(1-W)*exp(-scol*Dcol[j]))*vm_pdf_temp;
    Aa[j] = sum(vm_pdf_temp);
    
    Af[j] = exp(kappaf*cos(E[j,1]))/modified_bessel_first_kind(0,kappaf);
	  Ax[j] = Ac[j]+Af[j]/M+(a*Aa[j]+b*Ab[j])*q;
	  theta[j] = Ax[j]'/sum(Ax[j]);
    xpred[j] = categorical_rng(theta[j]);
  }
}
