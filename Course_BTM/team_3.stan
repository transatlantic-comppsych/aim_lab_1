data {
  int<lower=0> K; // players/items
  int<lower=0> G; // groups
  int<lower=0> N; // matches
  int<lower = 0> J; // judges
  
  array[N] int<lower=1, upper=K> player0; // team 0 players
  array[N] int<lower=1, upper=K> player1; // team 1 players
  array[N] int<lower = 1, upper = K> group0; // which group is player0 in?
  array[N] int<lower = 1, upper = K> group1; // which group is player1 in?
  array[N] int<lower = 1, upper = J> judge; // which judge is active?
  array[N] int<lower=0, upper=1> y; // winner
  
  array[J] real predictor; // mapping of predictor levels to each judge.
  
}
parameters {
  vector[K] weight_alpha ; //regression weight from predictor onto item-level ability
  vector[G] weight_gamma ; //regression weight from predictor onto group-level ability
  vector[K] intercept_alpha ; // regression intercept at item-level
  vector[G] intercept_gamma ; //regression intercept at group_level
}

model {
  weight_alpha ~ normal(0, 10);
  weight_gamma ~ normal(0, 10);
  intercept_alpha ~ normal (0, 10);
  intercept_gamma ~ normal(0, 10);
  
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(
      intercept_alpha[player1[n]] + predictor[judge[n]] * weight_alpha[player1[n]] 
    - intercept_alpha[player0[n]] + predictor[judge[n]] * weight_alpha[player0[n]]
    + intercept_gamma[group1[n]] + predictor[judge[n]] * weight_gamma[group1[n]]
    - intercept_gamma[group0[n]] + predictor[judge[n]] * weight_gamma[group0[n]]);
  }
}

