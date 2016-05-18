## @knitr pump-code

pumpCode <- nimbleCode({ 
  for (i in 1:N){

      # likelihood (data model)
      x[i] ~ dpois(lambda[i])

      # latent process (random effects)
      # linear predictor      
      lambda[i] <- theta[i]*t[i]
      # random effects distribution
      theta[i] ~ dgamma(alpha,beta)
  }
  # priors on hyperparameters
  alpha ~ dexp(1.0)
  beta ~ dgamma(0.1,1.0)
})

## @knitr pump-model

N <- 10
t <- c(94.3, 15.7, 62.9, 126, 5.24, 31.4, 1.05, 1.05, 2.1, 10.5)
x <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
pumpConsts <- list(t = t, N = 10)
pumpData <- list(x = x)
pumpInits <- list(alpha = 1, beta = 1,
         theta = rep(0.1, pumpConsts$N))
pump <- nimbleModel(pumpCode, 
          data = pumpData, constants = pumpConsts, inits = pumpInits)

## @knitr pump-compile

Cpump <- compileNimble(pump)

## @knitr pump-conf

pumpConf <- configureMCMC(pump, print = TRUE)
pumpConf$addMonitors(c('alpha', 'beta', 'theta'))


