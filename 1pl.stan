data {
  int<lower=1> J;              // Anzahl an Personen 
  int<lower=1> K;              // Anzahl an Items
  int<lower=1> N;              // Anzahl Beobachtungen (J*K)
  int<lower=1, upper=J> j[N];  // Personenindex der Nten Beobachtung  
  int<lower=1, upper=K> k[N];  // Itemindex der Nten Beobachtung
  int<lower=0, upper=1> y[N];  // response   
}

parameters {
  real theta[J];      // Personenfähigkeit
  real beta[K];       // Itemschwierigkeit
}

model {
  theta ~ normal(0, 1); 
  beta ~ normal(0, 10);

  for (i in 1:N)
    y[i] ~ bernoulli_logit(theta[j[i]] - beta[k[i]]); 
}

generated quantities {
  int y_rep[N];
  real theta_rep[J];

  for (i in 1:J) 
    theta_rep[i] = normal_rng(0, 1);

  for (i in 1:N)
    y_rep[i] = bernoulli_logit_rng(theta_rep[j[i]] - beta[k[i]]);
} 
