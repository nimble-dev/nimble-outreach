## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)

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

pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233, 209, 226, 192, 191, 225,
        245, 205, 191, 174, rep(NA, pyears))
year <- 1990:(2009 + pyears)

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))
## NIMBLE will handle y as data, T as a constant

set.seed(1)
inits <- function(){list(sigma.proc = runif(1, 0, 1), mean.r = rnorm(1),
                         sigma.obs = runif(1, 0, 1), logN.est = c(rnorm(1, 5.6, 0.1),
                                                         rep(NA, (length(year)-1))))}
ssm <- nimbleModel(ssmCodeAlt, constants = bugs.data, inits = inits()) 

## ------------------------------------------------------------------------
## buildAuxiliaryFilter is "just" a nimbleFunction
auxFilterSSM <- buildAuxiliaryFilter(ssm, nodes = 'logN.est')
compiled <- compileNimble(ssm, auxFilterSSM)
compiled$auxFilterSSM$run(10000) ## 10000 is the number of samples

## ---- fig.cap = ""-------------------------------------------------------
pmcmcConf <- configureMCMC(ssm, nodes = NULL,     # an empty configuration
                           monitors = c('mean.r','sigma.obs','sigma.proc')) 
pmcmcConf$addSampler(target = 'mean.r', type = 'RW_PF',
                      control = list(filterType = 'bootstrap', latents = 'logN.est'))
pmcmcConf$addSampler(target = 'sigma.proc', type = 'RW_PF',
                      control = list(filterType = 'bootstrap', latents = 'logN.est'))
pmcmcConf$addSampler(target = 'sigma.obs', type = 'RW_PF',
                      control = list(filterType = 'bootstrap', latents = 'logN.est'))
pmcmc <- buildMCMC(pmcmcConf)
Cpmcmc <- compileNimble(pmcmc, project = ssm, resetFunctions = TRUE)
Cpmcmc$run(1000)
plot(as.matrix(Cpmcmc$mvSamples)[,'mean.r'],
               xlab = 'iteration', ylab = 'mean.r')
plot(as.matrix(Cpmcmc$mvSamples)[,'sigma.obs'],
               xlab = 'iteration', ylab = 'sigma.obs')

## ------------------------------------------------------------------------
wrapAPF <- nimbleFunction(
   setup = function(model, ..., m = 10000) {
     APF <- buildAuxiliaryFilter(model, ...)
   },
   run = function() {
      returnType(double())
      return(APF$run(m))
   })

## example use:
wrapAPFssm <- wrapAPF(ssm, nodes = 'logN.est', m = 10000)
## this object won't be used further and was just shown to illustrate.

## ------------------------------------------------------------------------
RW_llFunction_new <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ###  control list extraction  ###
        adaptive       <- control$adaptive
        adaptInterval  <- control$adaptInterval
        scale          <- control$scale
        llFunction     <- control$llFunction
        includesTarget <- control$includesTarget
        ###  node list generation  ###
        calcNodes <- model$getDependencies(target)
        ###  nested function and function list definitions  ###
        mvInternal <- modelValues(model)
        RWControl <- list(adaptive = adaptive, adaptInterval = adaptInterval, scale = scale, logScale = FALSE, reflective = FALSE)
        targetRWSamplerFunction <- sampler_RW(model, mvInternal, target, RWControl)
        my_setAndCalculateOne <- setAndCalculateOne(model, target)
        my_decideAndJump <- decideAndJump(model, mvSaved, calcNodes)
    },

    run = function() {
        modelLP0 <- llFunction$run()
        if(!includesTarget)     modelLP0 <- modelLP0 + getLogProb(model, target)
        propValue <- rnorm(1, mean = model[[target]], sd = scale)
        my_setAndCalculateOne$run(propValue)
        modelLP1 <- llFunction$run()
        if(!includesTarget)     modelLP1 <- modelLP1 + getLogProb(model, target)
        jump <- my_decideAndJump$run(modelLP1, modelLP0, 0, 0)
        if(adaptive) {
            targetRWSamplerFunction$adaptiveProcedure(jump)
            scale <<- targetRWSamplerFunction$scale
        }
    },

    methods = list(
        reset = function() {
            targetRWSamplerFunction$reset()
        }
    )
)

## ------------------------------------------------------------------------
pmcmcConf2 <- configureMCMC(ssm, nodes = NULL, # an empty configuration
                            monitors = c('mean.r','sigma.obs','sigma.proc')) 
pmcmcConf2$addSampler(target = 'mean.r', type = RW_llFunction_new,
                      control = list(llFunction = wrapAPFssm, includesTarget = FALSE))
pmcmcConf2$addSampler(target = 'sigma.proc', type = RW_llFunction_new,
                      control = list(llFunction = wrapAPFssm, includesTarget = FALSE))
pmcmcConf2$addSampler(target = 'sigma.obs', type = RW_llFunction_new,
                      control = list(llFunction = wrapAPFssm, includesTarget = FALSE))

## ------------------------------------------------------------------------
pmcmc2 <- buildMCMC(pmcmcConf2)
Cpmcmc2 <- compileNimble(pmcmc2, project = ssm, resetFunctions = TRUE)

## ---- fig.cap=TRUE-------------------------------------------------------
Cpmcmc2$run(1000)
plot(as.matrix(Cpmcmc2$mvSamples)[,'mean.r'],
      xlab = 'iteration', ylab = 'mean.r')
plot(as.matrix(Cpmcmc2$mvSamples)[,'sigma.obs'],
      xlab = 'iteration', ylab = 'sigma.obs')

