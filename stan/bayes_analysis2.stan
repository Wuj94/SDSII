//functions {

// for likelihood estimation
  //real dirichlet_multinomial_lpmf(int[] y, vector alpha) {
  //  real alpha_plus = sum(alpha);
  //  return lgamma(alpha_plus) + sum(lgamma(alpha + to_vector(y)))
  //              - lgamma(alpha_plus+sum(y)) - sum(lgamma(alpha));
  //}
//}

data {
	int<lower=1> N;// number of observation => it's a consequence of a
					// group by operation
	int<lower=2> n_sub_event;
	int ds[N, n_sub_event];
}

parameters {
	simplex[n_sub_event] theta; // vector of Multinom distro para
	real<lower=0> sd_prior; 
	simplex[n_sub_event] alpha; // vector of Dir params
}

model {
	// priors
	sd_prior ~ cauchy(5,0.1) T[0,]; // each city has same sd_prior[i] * UnitMatrix
	alpha ~ normal(0, sd_prior);
	theta ~ dirichlet(alpha);
	// likelihood
	for(n in 1:N) {
		  ds[n,] ~ multinomial(theta);
	}
}