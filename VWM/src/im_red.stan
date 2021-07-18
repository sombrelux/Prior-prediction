functions{
  row_vector von_mises_pdf0(vector resp, real targ, real kappa){
    int N = num_elements(resp);
    row_vector[N] vm_pdf;
    
    vm_pdf = exp(kappa*cos(resp'-targ))/modified_bessel_first_kind(0,kappa);
    
    return vm_pdf;
  }
  matrix von_mises_pdf(vector resp, row_vector targ, real kappa){
    int M = num_elements(targ);
    int N = num_elements(resp);
    matrix[M, N] errors;
    matrix[M,N] vm_pdf;
    
    errors = rep_matrix(resp, M) - rep_matrix(targ, N);
    vm_pdf = exp(kappa*cos(errors'))/modified_bessel_first_kind(0,kappa);
    
    return vm_pdf;
  }
  
  real partial_sum(int[] x_slice, int start, int end, real[,] theta, int N) {
    real ctg_temp = 0.0;
    vector[N] theta_temp;
    int  len = end-start+1;
    
    for(j in 1:len){
      theta_temp = to_vector(theta[start+j-1]);
      ctg_temp += categorical_lpmf(x_slice[j]|theta_temp);
    }
    return ctg_temp;
  }
  
}
data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  int<lower=1,upper=nPart> ID[nTrial];
  int<lower=1,upper=8> Setsize[nTrial];
  int<lower=1> N; //360 potential answer
  int<lower=1> M; //max set size
  vector<lower=0,upper=pi()>[M] D[nTrial]; //distance of location feature, the first element is the location of the target
  
  vector<lower=-pi(),upper=pi()>[N] X; //possible responses
  row_vector<lower=-pi(),upper=pi()>[M] m[nTrial]; //color of stimulus, the first element is the color of the target
  
  int<lower=1,upper=N> x[nTrial]; //color of response
}
parameters{
  // 6 parameters of IM, c=1
  real<lower=0> a[nPart];
  real<lower=0> b[nPart];
  real<lower=0> kappa[nPart];
  real<lower=0> kappaf[nPart];
  real<lower=0> s[nPart];
  real<lower=0,upper=1> r[nPart];
}
transformed parameters{
  vector<lower=0,upper=1>[M] ind_temp;
  matrix<lower=0>[M, N] vm_pdf_temp;
  row_vector<lower=0>[N] Aa[nTrial]; //activation of features independent from location
  row_vector<lower=0>[N] Ab[nTrial]; //background noise
  row_vector<lower=0>[N] Ac[nTrial]; //activation associated with location feature
  row_vector<lower=0>[N] Af[nTrial]; //additional activation of attention
  row_vector<lower=0>[N] Ax[nTrial]; //total activation
  real<lower=0,upper=1> theta[nTrial,N];//simplex[N] theta[nTrial];

  for(j in 1:nTrial){
    Aa[j] = rep_row_vector(0.0, N);
    Ac[j] = rep_row_vector(0.0, N);
    vm_pdf_temp = von_mises_pdf(X,m[j],kappa[ID[j]]);
    if(Setsize[j] < M){
      ind_temp = append_row(rep_vector(1.0, Setsize[j]),rep_vector(0.0,M-Setsize[j]));
      Ac[j] = (ind_temp.*exp(-s[ID[j]]*D[j]))'*vm_pdf_temp;
      Aa[j] = ind_temp'*vm_pdf_temp;
    }
    else{
      Ac[j] = exp(-s[ID[j]]*D[j])'*vm_pdf_temp;
      Aa[j] = rep_row_vector(1.0,M)*vm_pdf_temp;
    }
    
    Af[j] = von_mises_pdf0(X,m[j,1],kappaf[ID[j]]);
    Ab[j] = rep_row_vector(Setsize[j]*1.0, N);
    Ax[j] = Ac[j]+1.0/Setsize[j]*Af[j]+(a[ID[j]]*Aa[j]+b[ID[j]]*Ab[j])*(Setsize[j]-1+r[ID[j]])/Setsize[j];
	
    theta[j] = to_array_1d(Ax[j]/sum(Ax[j]));
  }
}
model{
  int grainsize = 1;
  
  for(i in 1:nPart){
    //prior
    a[i] ~ normal(1,0.1)T[0,];
    b[i] ~ normal(1,0.1)T[0,];
    kappa[i]~ normal(1,0.1)T[0,];
    kappaf[i]~ normal(1,0.1)T[0,];
    s[i]~ normal(1,0.1)T[0,];
    r[i]~ beta(1,1);
  }
 
  //likelihood
  target += reduce_sum(partial_sum,x,grainsize,theta,N);
}
generated quantities{
  int<lower=1,upper=N> xpred[nTrial];
  vector<lower=0,upper=1>[N] theta_temp;
  
  for(j in 1:nTrial){
    theta_temp = to_vector(theta[j]);
    xpred[j] = categorical_rng(theta_temp);
  }
}
