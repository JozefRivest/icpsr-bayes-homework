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
  vector[p] gamma;
}
transformed parameters {
  //
  vector[N] lp_mean;
  vector[N] lp_phi;
  vector<lower=0>[N] mu;
  vector<lower=0>[N] phi;
  
  // Mean
  lp_mean[1 : N] = beta[1] + beta[2] * bmr_dem[1 : N];
  mu[1 : N] = exp(lp_mean[1 : N]);
  
  // Overdispersion
  lp_phi[1 : N] = gamma[1] + gamma[2] * bmr_dem[1 : N];
  phi[1 : N] = exp(lp_phi[1 : N]);
}
model {
  // Prior part of Bayesian inference
  target += normal_lpdf(beta[1] | 0, 10);
  target += normal_lpdf(beta[2] | 0, 10);
  
  target += normal_lpdf(gamma[1] | 0, 3);
  target += normal_lpdf(gamma[2] | 0, 3);
  
  // Likelihood part of Bayesian inference
  target += neg_binomial_2_lpmf(wgov_leadexp[1 : N] | mu[1 : N], phi[1 : N]);
}
generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;
  for (n in 1 : N) {
    log_lik[n] = neg_binomial_2_lpmf(wgov_leadexp[n] | mu[n], phi[n]);
    y_rep[n] = neg_binomial_2_rng(mu[n], phi[n]);
  }
}
