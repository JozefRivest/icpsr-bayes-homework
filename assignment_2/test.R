library(rstan)
library(bayesplot)
library(broom)
library(ggplot2)
library(tidybayes)

load("assignment_2/birthdata.Rda")

dat <- birth.data
head(dat)

model.list <- list(
  N = nrow(dat),
  birth_1 = dat$birth1,
  birth_2 = dat$birth2
)

fit <- stan(file = "assignment_2/model.stan", data = model.list)

print(fit)

trace_plot <- mcmc_trace(fit, pars = "p") +
  ggplot2::theme(
    text = element_text(size = 14)
  )
ggsave(
  "assignment_2/figures/trace_plot_1.png",
  plot = trace_plot,
  width = 7,
  height = 6
)

#############
## Problem 2
#############

model.list.2 <- list(
  N = nrow(dat),
  birth_1 = dat$birth1,
  birth_2 = dat$birth2
)

fit.2 <- stan(file = "assignment_2/model_2.stan", data = model.list.2)

print(fit.2)

trace_plot.2 <- mcmc_trace(fit.2, pars = c("p1", "p2")) +
  ggplot2::theme(
    text = element_text(size = 14)
  )
trace_plot.2
ggsave(
  "assignment_2/figures/trace_plot_2.png",
  plot = trace_plot.2,
  width = 9,
  height = 4
)

#############
## Problem 3
#############

dat <- as.data.frame(rstan::extract(fit.2))
names(dat)

print(fit.2)

plot.2 <- ggplot(dat) +
  geom_density(aes(p1, color = "p1", fill = "p1"), alpha = 0.4) +
  geom_density(aes(p2, color = "p2", fill = "p2"), alpha = 0.4) +
  scale_color_manual(name = "Variable", values = c(p1 = "red", p2 = "blue")) +
  scale_fill_manual(name = "Variable", values = c(p1 = "red", p2 = "blue")) +
  geom_segment(
    x = median(dat$p2),
    xend = median(dat$p2),
    y = 0,
    yend = 7,
    linetype = "dashed",
  ) +
  geom_segment(
    x = median(dat$p1),
    xend = median(dat$p1),
    y = 0,
    yend = 7,
    linetype = "dashed",
  ) +
  labs(y = "Density", x = " ") +
  theme_bw()

ggsave(
  "assignment_2/figures/post_dist.png",
  plot = plot.2,
  width = 9,
  height = 4
)

sum(dat$p1 < 0.66) / sum(dat$p1)
sum(dat$p2 > 0.66) / sum(dat$p2)
