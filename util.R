logLikelihood = function(y, theta) {
  #sum of log likelihoods = log of product of likelihoods
  sum( dmultinom(y, size=1, prob = theta) )
}

calculateDIC = function(y, theta_post, llFun) {
  #Calculate L
  theta_hat = apply(theta_post, 2, mean)
  L = llFun(y, theta_hat)
  
  #Calculate P
  S = nrow(theta_post) #S = number of iterations
  #Add up the log likelihoods of each iteration
  llSum = 0
  for (s in 1:S) {
    theta_s = theta_post[s,]
    llSum = llSum + llFun(y, theta_s)
  }
  P = 2 * (L - (1 / S * llSum))
  
  #Calculate DIC
  DIC = -2 * (L - P)
  
  #Return the results
  list(DIC=DIC, P=P, L=L)
}