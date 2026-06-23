set.seed(48104)

# Number of simulations
sims <- 1000
# Number of tria
trials <- 100
# Probability of success
p <- 0.5
# Define empty vector for U
u <- c()
# Define empty vector for V
v <- c()

for (i in 1:sims) {
  heads <- 0
  trials <- 0
  first_head <- NA

  while (heads < 2) {
    trials <- trials + 1
    flip <- rbinom(1, 1, p)
    if (flip == 1) {
      heads <- heads + 1
      if (heads == 1) first_head <- trials
    }
  }
  u[i] <- first_head
  v[i] <- trials
}

mean(u)
mean(v)

plot(
  jitter(u) ~ jitter(v),
  xlab = "Number of trials to get 2 heads",
  ylab = "Number of trials before 1st head"
)
abline(lm(u ~ v), col = "red")

############
## Monty hall problem
############

set.seed(48109)

sims <- 1000

car_behind_door <- rbinom(sims, 1, prob = 1 / 3)
lying <- rbinom(sims, 1, prob = 0.5)
monty_picks_door <- rbinom(sims, 1, prob = 0.5)

B <- (car_behind_door == 1 & lying == 0) |
  (car_behind_door == 0 & lying == 1 & monty_picks_door == 1)

mean(car_behind_door[B])
