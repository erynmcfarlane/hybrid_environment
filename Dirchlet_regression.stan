data {
  int<lower=1> N;             // Number of observations
  int<lower=1> K;             // Number of predictor variables
  int<lower=1> J;             // Number of populations
  vector<lower=0>[K] x[N];    // Predictor variables
  int<lower=1, upper=J> pop[N];// Population indicator for each observation
}

parameters {
  vector[K] beta[J];          // Population-specific coefficients
  vector<lower=0>[K] sigma[J]; // Population-specific standard deviations
  simplex[5] theta[J];        // Population-specific Dirichlet parameters
}

model {
  vector[5] p[J];              // Probability of each category in response variable
  
  for (j in 1:J) {
    beta[j] ~ normal(0, 1);    // Prior on population-specific coefficients
    sigma[j] ~ cauchy(0, 1);   // Prior on population-specific standard deviations
    theta[j] ~ dirichlet(rep_vector(1, 5));  // Prior on population-specific Dirichlet parameters
    
    for (n in 1:N) {
      if (pop[n] == j) {
        p[j] = softmax(x[n] .* beta[j]);  // Compute probabilities for each category
        target += dirichlet_lpdf(theta[j] | rep_vector(1, 5));  // Likelihood of Dirichlet parameters
        target += log(theta[j, pop[n]]);  // Likelihood of observed category
      }
    }
  }
}
