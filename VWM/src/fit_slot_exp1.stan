data{
  int<lower=1> nPart;
  int<lower=1> nTrial; 
  int<lower=1,upper=nPart> ID[nTrial];
  vector<lower=1,upper=8>[nTrial] Setsize;
  vector<lower=-pi(),upper=pi()>[nTrial] error;
}
parameters{
  real<lower=0,upper=8> K[nPart];
  real<lower=0> sigma1[nPart];
}
transformed parameters {
  real<lower=0> slotprop[nTrial];
  real<lower=1> wholeslots;
  real<lower=0,upper=1> rest;
  
  real<lower=0,upper=1> pm[nTrial];
  real<lower=0> sigma[nTrial];
  real<lower=0> R[nTrial];
  real<lower=0> kappa[nTrial];
  
  for(j in 1:nTrial){
    slotprop[j] = K[ID[j]]/Setsize[j];
    if(slotprop[j]<1){
      pm[j] = slotprop[j];//probability of the target item in memory
      sigma[j] = sigma1[ID[j]];
	  }
    else{
      pm[j] = 1;
  	  wholeslots = floor(slotprop[j]);
  	  rest = slotprop[j] - wholeslots;
  	  sigma[j] = (1-rest)*sigma1[ID[j]]/sqrt(wholeslots) + rest*sigma1[ID[j]]/sqrt(wholeslots+1);
    }
    
	  R[j] = exp(-pow(sigma[j],2)/2);
    if(R[j]>=0.85){
      kappa[j] = 1/(pow(R[j],3) - 4*pow(R[j],2) + 3*R[j]);
  	}
  	else if(R[j]>=0.53){
      kappa[j] = -0.4 + 1.39*R[j] + 0.43/(1-R[j]);
    }
    else{
      kappa[j] = 2*R[j] + pow(R[j],3) +5*pow(R[j],5)/6;
    }
  }
}
model{
  for(i in 1:nPart){
    K[i] ~ normal(4,1)T[0,8];
    sigma1[i] ~ normal(0,1)T[0,];
  }
  for(j in 1:nTrial){
    target += log_mix(pm[j],von_mises_lpdf(error[j]|0,kappa[j]),uniform_lpdf(error[j]|-pi(),pi()));
  }
}
