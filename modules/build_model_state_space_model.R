## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)

## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----model-code----------------------------------------------------------
library(nimble)
ssmCode <- nimbleCode({
# Priors and constraints
logN.est[1] ~ dnorm(5.6, 0.01)       # Prior for initial population size
mean.r ~ dnorm(1, 0.001)             # Prior for mean growth rate
sigma.proc ~ dunif(0, 1)             # Prior for sd of state process
sigma.obs ~ dunif(0, 1)              # Prior for sd of observation process

# Likelihood
# State process
for (t in 1:(T-1)){
   r[t] ~ dnorm(mean.r, sd = sigma.proc)
   logN.est[t+1] <- logN.est[t] + r[t]
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

## ----build-model---------------------------------------------------------
# Code from BPA book:
pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233, 209, 226, 192, 191, 225,
        245, 205, 191, 174, rep(NA, pyears))
year <- 1990:(2009 + pyears)

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))
## NIMBLE will handle y as data, T as a constant

# Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 1), mean.r = rnorm(1),
                         sigma.obs = runif(1, 0, 1), logN.est = c(rnorm(1, 5.6, 0.1),
                                                         rep(NA, (length(year)-1))))}

ssm <- nimbleModel(ssmCode, constants = bugs.data, inits = inits()) # inits handled differently 

## ---- compile-model------------------------------------------------------
Cssm <- compileNimble(ssm)

## ---- plot-graph, fig.cap=""---------------------------------------------
## Remove N.est[t] to reduce clutter 
ssmCode <- nimbleCode({
# Priors and constraints
logN.est[1] ~ dnorm(5.6, 0.01)       # Prior for initial population size
mean.r ~ dnorm(1, 0.001)             # Prior for mean growth rate
sigma.proc ~ dunif(0, 1)             # Prior for sd of state process
sigma.obs ~ dunif(0, 1)              # Prior for sd of observation process

# Likelihood
# State process
for (t in 1:(T-1)){
   r[t] ~ dnorm(mean.r, sd = sigma.proc)
   logN.est[t+1] <- logN.est[t] + r[t]
   }
# Observation process
for (t in 1:T) y[t] ~ dnorm(logN.est[t], sd = sigma.obs)

})

# Make a smaller version so the graph wil be readable
ssmSmall <- nimbleModel(ssmCode, constants = list(T = 5),
                        data = list(y = bugs.data$y[1:5]))
library(igraph)
graph <- ssmSmall$getGraph()
plot(graph, layout = layout_(graph, with_kk())) ## uses plot.igraph

