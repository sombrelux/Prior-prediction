data{
  int<lower=1> nTrial; 
  int<lower=1,upper=8> Setsize[nTrial];
  real<lower=-pi,upper=pi()> m[nTrial];
  real<lower=-pi(),upper=pi()> x[nTrial];
}
parameters{
  real<lower=0,upper=8> K;
  real<lower=0> sigma1;
}
transformed parameters {
  real<lower=0> slotprop[nTrial];
  real<lower=1> wholeslots;
  real<lower=0,upper=1> rest;
  
  real<lower=0,upper=1> pm[nTrial];
  real<lower=0> sigma[Trial];
  real<lower=0> R[nTrial];
  real<lower=0> kappa[nTrial];
  
  slotprop = K/Setsize;
  
  for(j in 1:nTrial){
    if(slotprop[j]<1){
      pm[j] = slotprop[j];//probability of the target item in memory
      sigma[j] = sigma1;
	}
    else{
      pm[j] = 1;
	  wholeslots = floor(slotprop[j]);
	  rest = slotprop[j] - wholeslots;
	  sigma[j] = (1-rest)*sigma1/sqrt(wholeslots) + rest*sigma1/sqrt(wholeslots+1);
    }
    
	R[j] = exp(-pow(sigma[j],2)/2);
    if(R[j]>=0.85){
      kappa[j] = 1/(pow(R[j],3) - 4*pow(R[j],2) + 3*R[j]);
	}
	else if(R[i,j]>=0.53){
      kappa[j] = -0.4 + 1.39*R[j] + 0.43/(1-R[j]);
    }
    else{
      kappa[j] = 2*R[j] + pow(R[j],3) +5*pow(R[j],5)/6;
    }
  }
}
model{
  K ~ normal(4,1)T[0,8];
  sigma1 ~ normal(0,1)T[0,];
  target += log_mix(pm,von_mises_lpdf(x|m,kappa),uniform_lpdf(x|m-pi(),m+pi()));
  //for(j in 1:nTrial){
    //target += log_mix(pm[j],von_mises_lpdf(x[j]|m[j],kappa[j]),uniform_lpdf(x[j]|m[j]-pi(),m[j]+pi()));
  //}
}
//generated quantities {
  //real<lower=-pi(),upper=3*pi()> xpost[nTrial];
  //xpost = von_mises_rng(m,z*kappa)
//}