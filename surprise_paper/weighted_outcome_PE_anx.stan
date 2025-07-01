
data {
  int<lower=1> N; // Number of Participants
  int<lower=1> T; // Trials per subject
  int<lower=1, upper=T> Tsubj[N]; 
  real Response_SubjPE[N, T];  // Reported expectation
  real Response_Ax[N, T];
  real Response_fdbk[N, T];
}
transformed data {
}
parameters {
  real intercept;
  real w1_mu;
  real w2_mu;
  real gam_mu;
  real sig_mu;
  
  real<lower=0> sigma_w0;
  vector<lower=0>[4] sigma;
  vector[N] w0_pr;
  vector[N] w1_pr;
  vector[N] w2_pr;
  vector[N] gam_pr;
  vector[N] sig_pr;
}
transformed parameters {
  vector[N] w0;
  vector[N] w1;
  vector[N] w2;
  vector<lower=0, upper=1>[N] gam;
  vector<lower=0>[N] sig;
  
  w0 = intercept + sigma_w0 * w0_pr;
  w1 = w1_mu + sigma[1] * w1_pr;
  w2 = w2_mu + sigma[2] * w2_pr;

  for (i in 1:N) {
    gam[i] = Phi_approx(gam_mu + sigma[3] * gam_pr[i]);
  }
  sig = exp(sig_mu + sigma[4] * sig_pr);
}
model {
   intercept  ~ normal(0, 1);
  w1_mu  ~ normal(0, 0.5);
  w2_mu  ~ normal(0, 0.5);
  gam_mu ~ normal(0, 0.5);
  sig_mu ~ normal(0, 0.5);
  
  sigma_w0 ~ normal(0, 1);
  sigma ~ normal(0, 0.1);

  // Individual parameters 
  w0_pr  ~ normal(0, 1.0);
  w1_pr  ~ normal(0, 1.0);
  w2_pr  ~ normal(0, 1.0);
  gam_pr ~ normal(0, 1.0);
  sig_pr ~ normal(0, 1.0);

  for (i in 1:N) {
    real outcome_sum = 0;

    for (t in 1:Tsubj[i]) {
      Response_Ax[i, t] ~ normal(w0[i] + w1[i] * outcome_sum + w2[i] * Response_SubjPE[i, t], sig[i]);

      outcome_sum += Response_fdbk[i, t];

      outcome_sum *= gam[i];
    }
  }
}
generated quantities {
  real mu_w0;
  real mu_w1;
  real mu_w2;
  real<lower=0, upper=1> mu_gam;
  real<lower=0> mu_sig;
  
  real log_lik[N];

  // For posterior predictive check
  real y_pred[N, T];
  
  // Set all posterior predictions to 0 (avoids NULL values)
  for (i in 1:N) {
    for (t in 1:T) {
      y_pred[i, t] = -1;
    }
  }

  mu_w0    = intercept;
  mu_w1    = w1_mu;
  mu_w2    = w2_mu;
  mu_gam   = Phi_approx(gam_mu);
  mu_sig   = exp(sig_mu);
  
  { // local section, this saves time and space
  for (i in 1:N) {
    real outcome_sum = 0;
    log_lik[i] = 0;

    for (t in 1:Tsubj[i]) {
      log_lik[i] += normal_lpdf(Response_Ax[i, t] | w0[i] + w1[i] * outcome_sum + w2[i] * Response_SubjPE[i, t], sig[i]);
      y_pred[i, t] = normal_rng(w0[i] + w1[i] * outcome_sum + w2[i] * Response_SubjPE[i, t], sig[i]);

      outcome_sum += Response_fdbk[i, t];

      outcome_sum *= gam[i];
    }
  }
  }
}
