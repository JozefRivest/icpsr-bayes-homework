data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> birth_1;
  array[N] int<lower=0, upper=1> birth_2;
}
parameters {
  real<lower=0, upper=1> p;
}
model {
  p ~ beta(1, 1);
  birth_1 ~ bernoulli(p);
  birth_2 ~ bernoulli(p);
}
