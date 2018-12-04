data {
  int<lower=1> J; // n representatives
  int<lower=1> M; // n proposals
  int<lower=1> N; // n obs
  int<lower=1, upper=J> j[N]; // representative for observation n
  int<lower=1, upper=M> m[N]; // proposal for observation n
  int<lower=0, upper=1> y[N]; // vote of observation n
}
parameters {
  real alpha[M];
  real beta[M];
  real theta[J];
}
//transformed parameters {
//  vector[N] mu;
//  for (i in 1:N)
//    mu[i] = theta[j[i]] * beta[m[i]] - alpha[m[i]];
//}
model {
  alpha ~ normal(0,5); // Item difficulties
  beta ~ normal(0,5);  // item discrimination
  theta ~ normal(0,1);
  theta[1] ~ normal(-1, .01); // Left-wing prior
  theta[2] ~ normal(1, .01);  // Right-wing prior
  // y ~ bernulli_logit(mu);
  for (n in 1:N)
    y[n] ~ bernoulli_logit(theta[j[n]] * beta[m[n]] - alpha[m[n]]);
}
// generated quantities {
//  vector[N] log_lik;
//  for (i in 1:N) {
//    log_lik[i] = bernoulli_logit_lpmf(y[i] | mu[i]);
//  }
// }
