data {
	int<lower=1> N; // number of observation
	int<lower=2> n_sub_event; // number of subevents

	matrix[N, n_sub_event] ds;
}

parameters {
	matrix[n_country, n_sub_event] alpha; // vector of Dir distro params
	matrix[n_country, n_sub_event] theta; // vector of Multinom distro para
	real<lower=0> sd_prior[n_country];	
}

transformed parameters {
	matrix[n_country, n_sub_event] theta_transf; // vector of logit -1 transformed vars
	for(n in 1:N) {
		for(c in 1:n_sub_event){
			theta_transf[n,c] = logit(theta[n,c]);
		}
	}
}

model {
	// priors
	for(c in 1:n_country){
		sd_prior[c] ~ cauchy(5,0) T[0,]; // each city has same sd_prior[i] * UnitMatrix
	}
	//alpha prior... those are half normal parameters
	for(c in 1:n_country){
		for(s in 1:n_sub_event){
			alpha[c,s] ~ normal(0, sd_prior[c]) T[0,];
		}
	}

	for(c in 1:n_country){
		transpose(theta[c]) ~ dirichlet(transpose(alpha[c]));
	}

	// likelihood
	
	sticazz ~ multinomial(transpose(theta[1]));
	/*for(n in 1:N) {
		for(c in 1:n_country){
		  transpose(ds[n,c]) ~ multinomial(transpose(theta[c]));
		}
	}*/
}