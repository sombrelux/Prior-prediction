functions{
  real solveKappa(real J0, real[] Jmap, real[] Kappamap,int Jl){
    real kappa0;
    int leftInd=0;
    while((J0>Jmap[leftInd+1])&&(leftInd<Jl)){
      leftInd += 1;
    }

    if (leftInd == 0)
      kappa0 = Kappamap[1];
    else if (leftInd == Jl)
      kappa0 = Kappamap[Jl];
    else
      kappa0 = Kappamap[leftInd] + (J0-Jmap[leftInd])*(Kappamap[leftInd+1]-Kappamap[leftInd])/(Jmap[leftInd+1]-Jmap[leftInd]);
    
    return kappa0;
  }
  real partial_sum(real[] x_slice, int start, int end, vector kappa) {
    real vm_lpdf = 0.0;
    int len = end-start+1;
    for(j in 1:len){
      vm_lpdf += von_mises_lpdf(x_slice[j]|0,kappa[start+j-1]);
    }
    return vm_lpdf;
  }
}
data{
  int<lower=1> L;
  int<lower=1> nTrial;
  vector<lower=1,upper=8>[nTrial] Setsize;
  real<lower=-pi(),upper=pi()> error[nTrial];
  real<lower=0> Jmap[L];
  real<lower=0> Kappamap[L];
}
parameters{
  real<lower=0> J1bar;
  real<lower=0> rate;
  real<lower=0> alpha;
}
transformed parameters{
  vector<lower=0,upper=1>[nTrial] R = 1./Setsize;
  vector<lower=0>[nTrial] shape;
  vector<lower=0>[nTrial] kappa;
  
  for(j in 1:nTrial){
      shape[j] = J1bar*rate*pow(R[j],alpha);
      kappa[j] = solveKappa(J[j],Jmap,Kappamap,L);
  }
}
model{
  int grainsize = 1;
  real<lower=0> J[nTrial];
  
  J1bar ~ normal(0,1)T[0,];
  rate ~ normal(0,1)T[0,];
  alpha ~ normal(0,1)T[0,];
  
  for(j in 1:nTrial){
    J[j] ~ gamma(shape[j],rate);
  }
  
  //Data
  target += reduce_sum(partial_sum, error,grainsize, kappa);  
}
