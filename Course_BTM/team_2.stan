/**
 * Hierarchical Bradley-Terry model for groups where each group is made of a fixed number of items.
   The mapping item -> Group does not change across contexts, and both item- and group abilities are inferred.
 */
data {
  int<lower=0> K; // players
  int<lower=0> G; // groups
  int<lower=0> N; // matches
  array[N] int<lower=1, upper=K> player0; // team 0 players
  array[N] int<lower=1, upper=K> player1; // team 1 players
  array[N] int<lower = 1, upper = K> group0; // which group is player0 in?
  array[N] int<lower = 1, upper = K> group1; // which group is player1 in?
  array[N] int<lower=0, upper=1> y; // winner
}
parameters {
  vector[K] alpha; // ability for player k
  vector[G] gamma; // ability for group g
  real<lower = 0> sigma_alpha; // how much variation is there at the player level
  real<lower = 0> sigma_gamma; // how much variation is there at the group level
}

transformed parameters {
  real ICC = sigma_gamma^2/(sigma_alpha^2 + sigma_gamma^2);  // estimate intra class coefficient

}

model {
  sigma_alpha ~ lognormal(0, 10); // weak priors for player ability
  sigma_gamma ~ lognormal(0, 10); // weak priors for player ability
  
  alpha ~ normal(0, sigma_alpha); 
  gamma ~ normal(0, sigma_gamma); 
  
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(alpha[player1[n]] + gamma[group1[n]] - alpha[player0[n]]- gamma[group0[n]]);
  }
}

