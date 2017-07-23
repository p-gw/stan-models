data {
  int<lower=1> J;              // number of persons 
  int<lower=1> K;              // number of items
  int<lower=1> N;              // number of observations (J*K)
  int<lower=1, upper=J> j[N];  // index for persons  
  int<lower=1, upper=K> k[N];  // index for items
  int<lower=0, upper=1> y[N];  // item response   
}
parameters {
  // person parameters
  real theta[J];      
  
  // item parameters
  real<lower=0> alpha[K];
  real<lower=0> mu_alpha;
  real<lower=0> sigma_alpha;

  real beta[K];       
  real mu_beta;
  real<lower=0> sigma_beta;
}
model {
  // person priors
  theta ~ normal(0, 1); 

  // item priors
  alpha ~ lognormal(mu_alpha, sigma_alpha);
  mu_alpha ~ normal(0, 10);
  sigma_alpha ~ cauchy(0, 5);

  beta ~ normal(mu_beta, sigma_beta);
  mu_beta ~ normal(0, 10);
  sigma_beta ~ cauchy(0, 5);

  for (i in 1:N)
    y[i] ~ bernoulli_logit(alpha[k[i]]*(theta[j[i]] - beta[k[i]])); 
}
