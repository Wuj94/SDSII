//functions {

// for likelihood estimation
  //real dirichlet_multinomial_lpmf(int[] y, vector alpha) {
  //  real alpha_plus = sum(alpha);
  //  return lgamma(alpha_plus) + sum(lgamma(alpha + to_vector(y)))
  //              - lgamma(alpha_plus+sum(y)) - sum(lgamma(alpha));
  //}
//}

data {
	int<lower=1> N[5];// number of observation for each of the 5 countries
	int<lower=2> n_sub_event;
	
	//I need a precompiler / preprocessor  AAAARGH 
	int ds1[N[1], n_sub_event];
	int ds2[N[2], n_sub_event];
	int ds3[N[3], n_sub_event];
	int ds4[N[4], n_sub_event];
	int ds5[N[5], n_sub_event];
}


parameters {
  real<lower=0> sd_prior; 
  
  //ds1
	simplex[n_sub_event] theta1; // vector of Multinom distro para
	simplex[n_sub_event] alpha1; // vector of Dir params
	//ds2
	simplex[n_sub_event] theta2; // vector of Multinom distro para
	simplex[n_sub_event] alpha2; // vector of Dir params
	//ds3
	simplex[n_sub_event] theta3; // vector of Multinom distro para
	simplex[n_sub_event] alpha3; // vector of Dir params
  //ds4
	simplex[n_sub_event] theta4; // vector of Multinom distro para
	simplex[n_sub_event] alpha4; // vector of Dir params
  //ds5
	simplex[n_sub_event] theta5; // vector of Multinom distro para
	simplex[n_sub_event] alpha5; // vector of Dir params
}

model {
  // ----- ds1 -----
	// priors
	sd_prior ~ cauchy(1, 1) T[0,]; // each city has same sd_prior[i] * UnitMatrix
	
	for(i in 1:n_sub_event){
	  alpha1[i] ~ normal(0, sd_prior) T[0,];
	}
	theta1 ~ dirichlet(alpha1);
	// likelihood
	for(n in 1:N[1]) {
		  ds1[n,] ~ multinomial(theta1);
	}
	
	// ----- ds2 -----
	// priors
	for(i in 1:n_sub_event){
	  alpha2[i] ~ normal(0, sd_prior) T[0,];
	}
	theta2 ~ dirichlet(alpha2);
	// likelihood
	for(n in 1:N[2]) {
		  ds2[n,] ~ multinomial(theta2);
	}
	
	// ----- ds3 -----
	// priors
	for(i in 1:n_sub_event){
	  alpha3[i] ~ normal(0, sd_prior) T[0,];
	}
	theta3 ~ dirichlet(alpha3);
	// likelihood
	for(n in 1:N[3]) {
		  ds3[n,] ~ multinomial(theta3);
	}
	
	// ----- ds4 -----
	// priors
	for(i in 1:n_sub_event){
	  alpha4[i] ~ normal(0, sd_prior) T[0,];
	}
	theta4 ~ dirichlet(alpha4);
	// likelihood
	for(n in 1:N[4]) {
		  ds4[n,] ~ multinomial(theta4);
	}
	
	// ----- ds5 -----
	// priors
	for(i in 1:n_sub_event){
	  alpha5[i] ~ normal(0, sd_prior) T[0,];
	}
	theta5 ~ dirichlet(alpha5);
	// likelihood
	for(n in 1:N[5]) {
		  ds5[n,] ~ multinomial(theta5);
	}
}
generated quantities {
  real riots[5];
  real protests[5];
  real vac[5];
  real explosions[5];
  
  riots[1] = (theta1[1] + theta1[3]) / sum(theta1);
  riots[2] = (theta2[1] + theta2[3]) / sum(theta2);
  riots[3] = (theta3[1] + theta3[3]) / sum(theta3);
  riots[4] = (theta4[1] + theta4[3]) / sum(theta4);
  riots[5] = (theta5[1] + theta5[3]) / sum(theta5);
  
  protests[1] = (theta1[2] + theta1[6]) / sum(theta1);
  protests[2] = (theta2[2] + theta2[6]) / sum(theta2);
  protests[3] = (theta3[2] + theta3[6]) / sum(theta3);
  protests[4] = (theta4[2] + theta4[6]) / sum(theta4);
  protests[5] = (theta5[2] + theta5[6]) / sum(theta5);
  
  vac[1] = (theta1[4] + theta1[5]) / sum(theta1);
  vac[2] = (theta2[4] + theta2[5]) / sum(theta2);
  vac[3] = (theta3[4] + theta3[5]) / sum(theta3);
  vac[4] = (theta4[4] + theta4[5]) / sum(theta4);
  vac[5] = (theta5[4] + theta5[5]) / sum(theta5);
  
  explosions[1] = (2*theta1[7]) / sum(theta1);
  explosions[2] = (2*theta2[7]) / sum(theta2);
  explosions[3] = (2*theta3[7]) / sum(theta3);
  explosions[4] = (2*theta4[7]) / sum(theta4);
  explosions[5] = (2*theta5[7]) / sum(theta5);
}