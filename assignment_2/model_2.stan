data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> birth_1;
  array[N] int<lower=0, upper=1> birth_2;
}
parameters {
  real<lower=0, upper=1> p1;
  real<lower=0, upper=1> p2;
}
model {
  p1 ~ beta(1, 1);
  p2 ~ beta(1, 1);
  for (i in 1 : N) {
    if (birth_1[i] == 0) {
      birth_2[i] ~ bernoulli(p1);
    } else if (birth_1[i] == 1) {
      birth_2[i] ~ bernoulli(p2);
    }
  }
  ;
}
