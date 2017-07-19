data {
	int<lower=1> J;			        // Anzahl an Personen 
	int<lower=1> K;			        // Anzahl an Items
	int<lower=1> N;			        // Anzahl Beobachtungen (J*K)
	int<lower=1> j[N];		      // Personenindex der Nten Beobachtung  
	int<lower=1> k[N];		      // Itemindex der Nten Beobachtung
	int<lower=0, upper=1> y[N];	// response 	
}

parameters {
	real theta[J]; 			      // Personenfähigkeit
	real beta[K]; 			      // Itemschwierigkeit
	real<lower=0> alpha[K];		// Diskrimination
}

model {
	theta ~ normal(0, 1);	
	beta ~ normal(0, 10);
	alpha ~ lognormal(0.5, 1);

	for (i in 1:N)
		y[i] ~ bernoulli_logit(alpha[k[i]]*(theta[j[i]] - beta[k[i]])); 
}
generated quantities {
	int y_tilde[N];
	real theta_tilde[J];

	for (i in 1:J)
		theta_tilde[i] = normal_rng(0, 1);

	for (i in 1:N)
		y_tilde[i] = bernoulli_rng(inv_logit(alpha[k[i]]*(theta[j[i]] - beta[k[i]])));
}
