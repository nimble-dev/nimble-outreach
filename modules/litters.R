code <- nimbleCode({
  for (i in 1:G) {
     for (j in 1:N) {
     	 # likelihood (data model)
        r[i,j] ~ dbin(p[i,j], n[i,j])
	# latent process (random effects)
        p[i,j] ~ dbeta(a[i], b[i]) 
     }
     # prior for hyperparameters
     a[i] ~ dgamma(1, .001)
     b[i] ~ dgamma(1, .001)
   }
})

consts <- list(G = 2,N = 16, n = matrix(c(13, 12, 12, 11, 9, 10, 
       9, 9, 8, 11, 8, 10, 13, 10, 12, 9, 10, 9, 10, 5, 9, 9, 13, 
       7, 5, 10, 7, 6, 10, 10, 10, 7), nrow = 2))
data = list(r = matrix(c(13, 12, 12, 11, 9, 10, 9, 9, 8, 10, 8, 9, 
     12, 9, 11, 8, 9, 8, 9, 4, 8, 7, 11, 4, 4, 5, 5, 3, 7, 3, 7, 0), 
     nrow = 2))
inits <- list( a = c(2, 2), b=c(2, 2) )


