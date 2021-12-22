data{
  int<lower=1> nPart;
  int<lower=1> nTrial;
  row_vector<lower=-1,upper=1>[nTrial] xs;
  row_vector[nTrial] xd;
  row_vector[nTrial] xr;
  row_vector<lower=-1,upper=1>[nTrial] ps;
  row_vector[nTrial] pd;
  row_vector[nTrial] pr;
  row_vector<lower=-1,upper=1>[nTrial] ts;
  row_vector[nTrial] td;
  row_vector[nTrial] tr;  
  
  real<lower=0> Ub;
}
generated quantities{
  vector<lower=0>[nPart] beta_xo;
  vector<lower=0>[nPart] beta_to;
  vector<lower=0>[nPart] beta_po;
  vector<lower=0>[nPart] beta_xa;
  vector<lower=0>[nPart] beta_pa;
  vector<lower=0>[nPart] beta_xr;
  vector<lower=0>[nPart] beta_pr;
  vector<lower=0>[nPart] beta_ta;
  vector<lower=0>[nPart] beta_tr;
  matrix[nPart,nTrial] X;
  matrix[nPart,nTrial] R;
  matrix[nPart,nTrial] TT;
  array[nTrial,nPart] real theta_logit;
  array[nTrial,nPart] int<lower=0,upper=1> ypred;
  
  for(k in 1:nPart){
    beta_xo[k] = uniform_rng(0,Ub);
    beta_xa[k] = uniform_rng(0,Ub);
    beta_xr[k] = uniform_rng(0,Ub);
    beta_po[k] = uniform_rng(0,Ub);
    beta_pa[k] = uniform_rng(0,Ub);
    beta_pr[k] = uniform_rng(0,Ub);
    beta_to[k] = uniform_rng(0,Ub);
    beta_ta[k] = uniform_rng(0,Ub);
    beta_tr[k] = uniform_rng(0,Ub);
    
    X[k] = beta_xo[k]*xs+beta_xa[k]*xd+beta_xr[k]*xr;
    R[k] = beta_po[k]*ps+beta_pa[k]*pd+beta_pr[k]*pr;
    TT[k] = beta_to[k]*ts+beta_ta[k]*td+beta_tr[k]*tr;
  }
  theta_logit = to_array_2d((X+R+TT)');
  for(j in 1:nTrial){
    ypred[j] = bernoulli_logit_rng(theta_logit[j]);
  }
}
