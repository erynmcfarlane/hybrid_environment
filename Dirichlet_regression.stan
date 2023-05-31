data {
  int<lower=1> N;             // Total number of observations
  int<lower=1> K;             // Number of predictor variables
  int<lower=1> J;             // Number of populations
  matrix[N,K] y;              // Response variables
  matrix[N, K] x;             // Predictor variables (continuous)
  int<lower=1> n[J];          // Sample sizes for each population
}

parameters {
  vector[K] beta[J];          // Population-specific regression coefficients
  real<lower=0> alpha[J];     // Population-specific Dirichlet hyperparameters
  real<lower=0> alpha0[K];    // Global hyperparameters for predictor variables
  real mu[K];                 // Global means for predictor variables
  real<lower=0> sigma[K];     // Global standard deviations for predictor variables
}

model {
  vector[K] theta[J];         // Probability parameters for Dirichlet distribution

  // Priors for global hyperparameters
  alpha0 ~ gamma(1, 1);       // Gamma prior for alpha0
  mu ~ normal(0, 10);         // Normal prior for mu
  sigma ~ cauchy(0, 5);       // Cauchy prior for sigma

  // Priors for population-specific hyperparameters
  for (j in 1:J) {
    alpha[j] ~ gamma(1, 1);   // Gamma prior for alpha[j]
    beta[j] ~ normal(mu, sigma);  // Multivariate normal prior for beta[j]
  }

  // Likelihood
  for (i in 1:N) {
    for (j in 1:J) {
      theta[j] = softmax(beta[j] .* x[i]');
     }
     y[i] ~ dirichlet_lpdf(rep_vector(theta[J], i));  /// there's a lot I don't understand, but this is the line that is fucking me up.
  }
}

generated quantities {
  vector[K] beta_global;       // Global estimate of regression coefficients

  // Compute the global estimate of regression coefficients
  for (k in 1:K) {
    beta_global[k] = mean(beta[:, k]);
  }
}
