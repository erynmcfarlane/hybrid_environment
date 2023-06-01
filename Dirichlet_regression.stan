data {
  int<lower=1> N;             // Total number of individuals
  int<lower=1> K;             // Number of predictor variables
  int<lower=1> J;             // Number of populations
  matrix[N, K] x;             // Predictor variables (continuous)
  //int<lower=1> n[J];          // Sample sizes for each population
}

parameters {
  simplex[J] theta[N];        // Probability parameters for Dirichlet distribution
  matrix[J, K] beta;          // Population-specific regression coefficients
  vector<lower=0>[J] alpha;   // Population-specific Dirichlet hyperparameters
  vector<lower=0>[K] alpha0;  // Global hyperparameters for predictor variables
  vector[K] mu;               // Global means for predictor variables
  vector<lower=0>[K] sigma;   // Global standard deviations for predictor variables
}

model {
  // Priors for global hyperparameters
  alpha0 ~ gamma(1, 1);       // Gamma prior for alpha0
  mu ~ normal(0, 10);         // Normal prior for mu
  sigma ~ cauchy(0, 5);       // Cauchy prior for sigma

  // Priors for population-specific hyperparameters
  alpha ~ gamma(1, 1);        // Gamma prior for alpha
  for (j in 1:J) {
    beta[j, ] ~ multi_normal(mu, diag_matrix(sigma));  // Multivariate normal prior for beta[j, ]
  }

  // Likelihood
 for (i in 1:N) {
    vector[J] p;
    for (j in 1:J) {
      p[j] = exp(x[i] * beta[j]');
    }
    p = p / sum(p);  // Normalize probabilities
   theta[i] ~ dirichlet(alpha + p);
  }
}

generated quantities {
  matrix[K, J] beta_global;       // Global estimate of regression coefficients

  // Compute the global estimate of regression coefficients
  for (k in 1:K) {
    for (j in 1:J) {
      beta_global[k, j] = beta[j, k];
    }
  }
}
