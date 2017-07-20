data {
  int<lower=1> J;              // number of persons 
  int<lower=1> K;              // number of items
  int<lower=1> N;              // number of observations (J*K)
  int<lower=1, upper=J> j[N];  // index for persons  
  int<lower=1, upper=K> k[N];  // indey for items
  int<lower=0, upper=1> y[N];  // item response   
}
parameters {
  // person parameters
  vector[J] theta;  

  // item parameters
  vector[K] beta_std;
  real mu_beta;  
  real<lower=0> sigma_beta;
}
transformed parameters {
  vector[K] beta;

  beta = mu_beta + beta_std*sigma_beta;
}
model {
  // persion priors (model identification)
  theta ~ normal(0, 1); 

  // item priors
  beta_std ~ normal(0, 1);
  mu_beta ~ normal(0, 10);
  sigma_beta ~ cauchy(0, 5); 

  y ~ bernoulli_logit(theta[j] - beta[k]); 
}
generated quantities {
  int<lower=0, upper=1> y_rep[N];
  real theta_rep[J];

  for (i in 1:J) 
    theta_rep[i] = normal_rng(0, 1);

  for (i in 1:N)
    y_rep[i] = bernoulli_logit_rng(theta_rep[j[i]] - beta[k[i]]);
} 
