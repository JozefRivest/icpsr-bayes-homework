// Poisson model example

data {
  // Define variables in data
  // Number of observations (an integer)
  int<lower=0> N;
  // Number of beta parameters
  int<lower=0> p;
  
  // Covariates
  vector<lower=0, upper=1>[N] bmr_dem;
  
  // Count outcome
  array[N] int<lower=0> wgov_leadexp;
}
parameters {
  // Define parameters to estimate
  vector[p] beta;
}
transformed parameters {
  //
  vector[N] lp;
  vector<lower=0>[N] mu;
  
  // linear predictor
  lp[1 : N] = beta[1] + beta[2] * bmr_dem[1 : N];
  
  // Mean
  mu[1 : N] = exp(lp[1 : N]);
}
model {
  // Prior part of Bayesian inference
  target += normal_lpdf(beta[1] | 0, 10);
  target += normal_lpdf(beta[2] | 0, 10);
  
  // Likelihood part of Bayesian inference
  target += poisson_lpmf(wgov_leadexp | mu[1 : N]);
}
generated quantities {
  // array[N] real y_rep = poisson_rng(mu);
  vector[N] log_lik;
  for (n in 1 : N) 
    log_lik[n] = poisson_lpmf(wgov_leadexp[n] | mu[n]);
}
