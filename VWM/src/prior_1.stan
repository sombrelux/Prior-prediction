functions{
  real von_mises_pdf(real resp, real targ, real kappa){
    real vm_pdf;
    vm_pdf = exp(kappa*cos(resp-targ))/modified_bessel_first_kind(0,kappa);
  return vm_pdf;
  }
}
data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  int<lower=1,upper=nPart> ID[nTrial];
  int<lower=1> M; //max set size
  int<lower=1> N; //360 potential answer
  int<lower=1,upper=M> Setsize[nTrial];
  vector<lower=0,upper=2*pi()>[N] X; //possible responses
  
  vector<lower=0,upper=pi()>[M] D[nTrial]; //distance of location feature, the first element is the location of the target
  vector<lower=0,upper=2*pi()>[M] m[nTrial]; //color of stimulus, the first element is the color of the target
}
parameters{
  //individual parameters
  real<lower=0,upper=1> a[nPart];
  real<lower=0,upper=1> b[nPart];
  real<lower=0,upper=1> r[nPart];
  real<lower=5,upper=20> s[nPart];
  vector<lower=5,upper=15>[nPart] kappa;
  vector<lower=0,upper=5>[nPart] delta;
  
}
transformed parameters{
  vector<lower=5,upper=20>[nPart] kappaf;
  simplex[N] theta[nTrial];
  vector<lower=0>[N] Aa[nTrial]; //activation of features independent from location
  vector<lower=0>[N] Ab[nTrial]; //background noise
  vector<lower=0>[N] Ac[nTrial]; //activation associated with location feature
  vector<lower=0>[N] Af[nTrial]; //additional activation of attention
  vector<lower=0>[N] Ax[nTrial]; //total activation
  real<lower=0> vm_temp; //unnormalized vm_pdf
  real<lower=0, upper=1> p; //prob of focus at target
  real<lower=0, upper=1> q;
  
  kappaf = kappa+delta;
  for(j in 1:nTrial){
    p = 1.0/Setsize[j];
    q = 1-p;
    for(k in 1:N){
      Aa[j,k] = 0.0;
      Ac[j,k] = 0.0;
      
	  for(l in 1:Setsize[j]){
	    vm_temp = von_mises_pdf(X[k],m[j,l],kappa[ID[j]]);
        Ac[j,k] += exp(-s[ID[j]]*D[j,l])*vm_temp;
        Aa[j,k] += vm_temp;
      }
      Af[j,k] = von_mises_pdf(X[k],m[j,1],kappaf[ID[j]]);
    }
	  Ab[j] = rep_vector(Setsize[j]*1.0,N);
	  Ax[j] = Ac[j]+p*Af[j]+(a[ID[j]]*Aa[j]+b[ID[j]]*Ab[j])*(q+p*r[ID[j]]);
    theta[j] = Ax[j]/sum(Ax[j]);
  }  
}
model{
  for(i in 1:nPart){
    //prior
    a[i] ~ beta(1,1);
    b[i] ~ beta(1,1);
    r[i]~ beta(1,1);
    s[i]~ uniform(5,20);
    kappa[i]~ uniform(5,15);
    delta[i]~ uniform(0,5);    
  }
}
generated quantities{
  real<lower=1,upper=360> ypred[nTrial];
  for(j in 1:nTrial){
    ypred[j] = categorical_rng(X,theta[j]);
  }
}
