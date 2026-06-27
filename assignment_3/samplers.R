library(rstan)
library(tidyverse)
library(loo)
library(bridgesampling)

# Loading the data
qog <- haven::read_dta("assignment_3/qog_std_cs_jan26.dta")

# Generate the draws
model.binom <- stan_model("assignment_3/negbin.stan", verbose = T)
model.poisson <- stan_model("assignment_3/poisson-model.stan", verbose = T)

# create a subset of the data that is used for the sampler
tmp <- qog |>
  select(wgov_leadexp, bmr_dem) |>
  filter(!is.na(wgov_leadexp)) |>
  filter(!is.na(bmr_dem))

# draw samples from the model
fit.poisson <- sampling(
  model.poisson,
  data = list(
    N = dim(tmp)[1],
    p = 2,
    bmr_dem = tmp$bmr_dem,
    wgov_leadexp = tmp$wgov_leadexp
  ), # data fed into the model
  seed = 654321, # RNG seed for replicability
  iter = 5000, # number of samples to draw per chain
  warmup = 2000, # number of discarded warmup samples/chain
  chains = 4,
  cores = 4, # number of independent samplers
  refresh = 1000 # how often to report sampler progress
)

fit.nb <- sampling(
  model.binom,
  data = list(
    N = dim(tmp)[1],
    p = 2,
    bmr_dem = tmp$bmr_dem,
    wgov_leadexp = tmp$wgov_leadexp
  ), # data fed into the model
  seed = 654321, # RNG seed for replicability
  iter = 5000, # number of samples to draw per chain
  warmup = 2000, # number of discarded warmup samples/chain
  chains = 4,
  cores = 4, # number of independent samplers
  refresh = 1000 # how often to report sampler progress
)

## Comparing models
loo_compare(loo(fit.nb), loo(fit.poisson), loo(fit.negbin))

poisson.bridge <- bridge_sampler(samples = fit.poisson)
nb.bridge <- bridge_sampler(samples = fit.nb)
negbin.bridge <- bridge_sampler(samples = fit.negbin)

bridgesampling::bf(poisson.bridge, nb.bridge, log = T)
bridgesampling::bf(poisson.bridge, negbin.bridge, log = T)
bridgesampling::bf(nb.bridge, negbin.bridge, log = T)
