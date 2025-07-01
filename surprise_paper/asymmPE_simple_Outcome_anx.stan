data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N]; 
  real Response_fdbk[N, T];
  real Response_Ax[N, T];
  real Response_PosPE[N, T];
  real Response_NegPE[N, T];
}
parameters {
  real intercept;
  real w1_mu;
  real w_pos_mu;
  real w_neg_mu;
  real gam_mu;
  real sig_mu;

  real<lower=0> sigma_w0;
  vector<lower=0>[5] sigma;

  vector[N] w0_pr;
  vector[N] w1_pr;
  vector[N] w_pos_pr;
  vector[N] w_neg_pr;
  vector[N] gam_pr;
  vector[N] sig_pr;
}
transformed parameters {
  vector[N] w0;
  vector[N] w1;
  vector[N] w_pos;
  vector[N] w_neg;
  vector<lower=0, upper=1>[N] gam;
  vector<lower=0>[N] sig;

  w0 = intercept + sigma_w0 * w0_pr;
  w1 = w1_mu + sigma[1] * w1_pr;
  w_pos = w_pos_mu + sigma[2] * w_pos_pr;
  w_neg = w_neg_mu + sigma[3] * w_neg_pr;

  for (i in 1:N) {
    gam[i] = Phi_approx(gam_mu + sigma[4] * gam_pr[i]);
  }
  sig = exp(sig_mu + sigma[5] * sig_pr);
}
model {
  intercept ~ normal(0, 1);
  w1_mu ~ normal(0, 0.5);
  w_pos_mu ~ normal(0, 0.5);
  w_neg_mu ~ normal(0, 0.5);
  gam_mu ~ normal(0, 0.5);
  sig_mu ~ normal(0, 0.5);

  sigma_w0 ~ normal(0, 1);
  sigma ~ normal(0, 0.1);

  w0_pr ~ normal(0, 1);
  w1_pr ~ normal(0, 1);
  w_pos_pr ~ normal(0, 1);
  w_neg_pr ~ normal(0, 1);
  gam_pr ~ normal(0, 1);
  sig_pr ~ normal(0, 1);

  for (i in 1:N) {
    real pospe_sum = 0;
    real negpe_sum = 0;

    for (t in 1:Tsubj[i]) {
      Response_Ax[i, t] ~ normal(
        w0[i] + w1[i] * Response_fdbk[i, t] + w_pos[i] * pospe_sum + w_neg[i] * negpe_sum,
        sig[i]);

      pospe_sum += Response_PosPE[i, t];
      negpe_sum += Response_NegPE[i, t];

      pospe_sum *= gam[i];
      negpe_sum *= gam[i];
    }
  }
}
generated quantities {
  real mu_w0;
  real mu_w1;
  real mu_w_pos;
  real mu_w_neg;
  real<lower=0, upper=1> mu_gam;
  real<lower=0> mu_sig;

  real log_lik[N];
  real y_pred[N, T];

  mu_w0 = intercept;
  mu_w1 = w1_mu;
  mu_w_pos = w_pos_mu;
  mu_w_neg = w_neg_mu;
  mu_gam = Phi_approx(gam_mu);
  mu_sig = exp(sig_mu);

  for (i in 1:N) {
    real pospe_sum = 0;
    real negpe_sum = 0;
    log_lik[i] = 0;

    for (t in 1:Tsubj[i]) {
      real mu = w0[i] + w1[i] * Response_fdbk[i, t] + w_pos[i] * pospe_sum + w_neg[i] * negpe_sum;
      log_lik[i] += normal_lpdf(Response_Ax[i, t] | mu, sig[i]);
      y_pred[i, t] = normal_rng(mu, sig[i]);

      pospe_sum += Response_PosPE[i, t];
      negpe_sum += Response_NegPE[i, t];

      pospe_sum *= gam[i];
      negpe_sum *= gam[i];
    }
  }
}

