library(rstan)
library(tidyverse)
library(patchwork)
library(bayesplot)

##
# Negative binomial
##

model.negbin <- stan_model("assignment_3/negative-binom.stan", verbose = T)

# create a subset of the data that is used for the sampler
tmp <- qog |>
  select(wgov_leadexp, bmr_dem) |>
  filter(!is.na(wgov_leadexp)) |>
  filter(!is.na(bmr_dem))

# draw samples from the model
fit.negbin <- sampling(
  model.negbin,
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

# Print the results
print(
  fit.negbin,
  pars = c(
    "beta[1]",
    "beta[2]",
    "gamma[1]",
    "gamma[2]"
  )
)

draws <- posterior::as_draws_df(fit.negbin)
head(draws)

aut <- mean(exp(draws$`beta[1]`))
print(aut)
dem <- mean(exp(draws$`beta[1]` + draws$`beta[2]`))
print(dem)


# Diagnostic
trace_2 <- mcmc_trace(
  fit.negbin,
  pars = c("beta[1]", "beta[2]", "gamma[1]", "gamma[2]")
)
trace_2
ggsave(
  "assignment_3/figures/trace_2.png",
  plot = trace_2,
  width = 9,
  height = 5
)

beta_1_dens <- bayesplot::mcmc_dens_chains(fit.negbin, pars = "beta[1]")
beta_2_dens <- bayesplot::mcmc_dens_chains(fit.negbin, pars = "beta[2]")
gamma_1_dens <- bayesplot::mcmc_dens_chains(fit.negbin, pars = "gamma[1]")
gamma_2_dens <- bayesplot::mcmc_dens_chains(fit.negbin, pars = "gamma[2]")

dens_chains <- (beta_1_dens + beta_1_dens) / (gamma_1_dens + gamma_2_dens)

ggsave(
  "assignment_3/figures/dens_chains.png",
  plot = dens_chains,
  width = 9,
  height = 5
)

acp <- bayesplot::mcmc_acf(
  fit.negbin,
  pars = c("beta[1]", "beta[2]", "gamma[1]", "gamma[2]")
)
ggsave(
  "assignment_3/figures/acp.png",
  plot = acp,
  width = 7,
  height = 5
)


dem <- tmp$bmr_dem
y_rep <- as.matrix(fit.negbin, pars = "y_rep")

yrep_long <- as_tibble(t(y_rep[1:50, ])) |> # N x 50 replicates
  mutate(group = dem) |>
  pivot_longer(-group, names_to = ".rep", values_to = "count")

post_pred <- ggplot(mapping = aes(count)) +
  geom_density(data = yrep_long, aes(group = .rep), color = "grey70") +
  geom_density(
    data = tibble(count = tmp$wgov_leadexp, group = dem),
    color = "black",
    linewidth = 1
  ) +
  facet_wrap(
    ~group,
    labeller = as_labeller(c(`0` = "non-democracy", `1` = "democracy"))
  ) +
  theme_bw()

ggsave(
  "assignment_3/figures/post_pred.png",
  post_pred,
  height = 5,
  width = 6
)
