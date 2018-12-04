data {
 int<lower=1> J; // N representatives
 int<lower=1> M; // N proposals
 int<lower=1> N; // N obs
 int<lower=1, upper=J> j[N]; // Representatives for observation n
 int<lower=1, upper=M> m[N]; // Proposals for observation n
 int<lower=0, upper=1> y[N]; // vote of observation n
}
parameters {
  real alpha[M];
  matrix[M,2] beta;
  matrix[J,2] theta;
}
model {
  alpha ~ normal(0, 10); 
  to_vector(beta) ~ normal(0, 10); 
  to_vector(theta) ~ normal(0, 1); 
  theta[1, 1] ~ normal(-1, .01); // Left-wing
  theta[2, 1] ~ normal(1, .01);  // Right-wing
  //beta[1, 2] ~ normal(-4, 2); 
  //beta[2, 2] ~ normal(4, 2); 
  //beta[1, 1] ~ normal(0, .1); 
  //beta[2, 1] ~ normal(0, .1); 
  for (n in 1:N)
    y[n] ~ bernoulli_logit(theta[j[n], 1] * beta[m[n], 1] + theta[j[n], 2] * beta[m[n], 2] - alpha[m[n]]);
}

