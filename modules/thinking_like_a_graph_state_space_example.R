## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)
read_chunk('chunks_ssm.R')

## ---- ssm-code-----------------------------------------------------------

## ---- ssm-model----------------------------------------------------------

## ------------------------------------------------------------------------
ssmCodeAlt <- nimbleCode({
# Priors and constraints
logN.est[1] ~ dnorm(5.6, 0.01)       # Prior for initial population size
mean.r ~ dnorm(1, 0.001)             # Prior for mean growth rate
sigma.proc ~ dunif(0, 1)             # Prior for sd of state process
sigma.obs ~ dunif(0, 1)              # Prior for sd of observation process

# Likelihood
# State process
for (t in 1:(T-1)){
   logN.est[t+1] ~ dnorm(logN.est[t] + mean.r, sd = sigma.proc)
   }
# Observation process
for (t in 1:T) {
   y[t] ~ dnorm(logN.est[t], sd = sigma.obs)
   }

# Population sizes on real scale
for (t in 1:T) {
   N.est[t] <- exp(logN.est[t])
   }
})

## ------------------------------------------------------------------------
pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233, 209, 226, 192, 191, 225,
        245, 205, 191, 174, rep(NA, pyears))
year <- 1990:(2009 + pyears)

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))
## NIMBLE will handle y as data, T as a constant


## ----eval=FALSE----------------------------------------------------------
## set.seed(1)
## ourInits <- inits()
## mcmcResult <- compareMCMCs(
##      list(ssm = list(code = ssmCode, inits = ourInits, data = bugs.data)),
##      niter = 50000, burnin = 5000, MCMCs = 'nimble', summary = TRUE)
## 
## mcmcResultAlt <- compareMCMCs(
##      list(ssm = list(code = ssmCodeAlt, inits = ourInits, data = bugs.data)),
##      niter = 50000, burnin = 5000, MCMCs = 'nimble', summary = TRUE)
## 
## mcmcResultAlt[[1]] <- rename_MCMC_comparison_method('nimble', 'nimble(alt)',
##                       mcmcResultAlt[[1]])
## 
## mcmcBoth <- combine_MCMC_comparison_results(mcmcResult[[1]], mcmcResultAlt[[1]])
## 
## make_MCMC_comparison_pages(mcmcBoth, dir = 'ssm_MCMC_comparison')

