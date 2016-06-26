## ----chunksetup, include=FALSE-------------------------------------------
# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)  # otherwise new() not being found - weird
library(nimble)

## ---- reflect-sampler----------------------------------------------------
sampler_RW_reflect <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        dist <- model$getNodeDistribution(target)
        targetComponents <- model$expandNodeNames(target, returnScalarComponents = TRUE)
        if(length(targetComponents) > 1)
                stop("sampler_RW_reflect: cannot use univariate RW sampler on multivariate target, try RW_block sampler.")
        rg <- getDistribution(dist)$range
        if(rg[1] > -Inf || rg[2] < Inf)
                  reflect <- TRUE else reflect <- FALSE

        calcNodes  <- model$getDependencies(target)
    },
    
    run = function() {
        propValue <- rnorm(1, mean = model[[target]], sd = scale)

        if(reflect) {
             if(propValue < rg[1]) propValue <- 2*rg[1] - propValue
             if(propValue > rg[2]) propValue <- 2*rg[2] - propValue
        }
 
        model[[target]] <<- propValue
        logMHR <- calculateDiff(model, calcNodes)
        jump <- decide(logMHR)
        if(jump)
            nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        else
            nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
    },
    methods = list(
            reset = function () {}
            )
)

## ---- Metr---------------------------------------------------------------
sampler_RW <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ###  control list extraction  ###
        adaptive      <- control$adaptive
        adaptInterval <- control$adaptInterval
        scale         <- control$scale
        ###  node list generation  ###
        targetAsScalar <- model$expandNodeNames(target, 
                       returnScalarComponents = TRUE)
        if(length(targetAsScalar) > 1)     
                        stop('more than one target; cannot use RW sampler, try RW_block sampler')
        calcNodes  <- model$getDependencies(target)
        ###  numeric value generation  ###
        scaleOriginal <- scale
        timesRan      <- 0
        timesAccepted <- 0
        timesAdapted  <- 0
        scaleHistory          <- c(0, 0)
        acceptanceRateHistory <- c(0, 0)
        ## variables previously inside of nested functions:
        optimalAR <- 0.44
        gamma1    <- 0
    },
    
    run = function() {
        propValue <- rnorm(1, mean = model[[target]], sd = scale)
     	model[[target]] <<- propValue
        logMHR <- calculateDiff(model, calcNodes)
        jump <- decide(logMHR)
        if(jump)
            nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        else
            nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        if(adaptive)     adaptiveProcedure(jump)
    },
    
    methods = list(
        
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                setSize(scaleHistory,          timesAdapted)
                setSize(acceptanceRateHistory, timesAdapted)
                scaleHistory[timesAdapted] <<- scale
                acceptanceRateHistory[timesAdapted] <<- acceptanceRate
                gamma1 <<- 1/((timesAdapted + 3)^0.8)
                gamma2 <- 10 * gamma1
                adaptFactor <- exp(gamma2 * (acceptanceRate - optimalAR))
                scale <<- scale * adaptFactor
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        
        reset = function() {
            scale <<- scaleOriginal
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            scaleHistory          <<- scaleHistory          * 0
            acceptanceRateHistory <<- acceptanceRateHistory * 0
            gamma1 <<- 0
        }
    ), where = getLoadingNamespace()
)


## ---- newSampler---------------------------------------------------------
sampler_RW_reflect <- nimbleFunction(
    contains = sampler_BASE,
    setup = function(model, mvSaved, target, control) {
        ###  control list extraction  ###
        adaptive      <- control$adaptive
        adaptInterval <- control$adaptInterval
        scale         <- control$scale
        ###  node list generation  ###
        targetAsScalar <- model$expandNodeNames(target, 
                       returnScalarComponents = TRUE)
        if(length(targetAsScalar) > 1)     
                       stop('more than one target; cannot use RW sampler, try RW_block sampler')

        ### ADDED code ############################################
        dist <- model$getNodeDistribution(target)
        rg <- getDistribution(dist)$range
        if(rg[1] > -Inf || rg[2] < Inf)
                  reflect <- TRUE else reflect <- FALSE
        ###########################################################

        calcNodes  <- model$getDependencies(target)
        ###  numeric value generation  ###
        scaleOriginal <- scale
        timesRan      <- 0
        timesAccepted <- 0
        timesAdapted  <- 0
        scaleHistory          <- c(0, 0)
        acceptanceRateHistory <- c(0, 0)
        ## variables previously inside of nested functions:
        optimalAR <- 0.44
        gamma1    <- 0
    },
    
    run = function() {
        propValue <- rnorm(1, mean = model[[target]], sd = scale)

        ### ADDED code ############################################
        if(reflect) {
             while(propValue < rg[1] | propValue > rg[2]) {
                   if(propValue < rg[1]) propValue <- 2*rg[1] - propValue
                   if(propValue > rg[2]) propValue <- 2*rg[2] - propValue
             }
        }
        ###########################################################

        model[[target]] <<- propValue
        logMHR <- calculateDiff(model, calcNodes)
        jump <- decide(logMHR)
        if(jump)
            nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        else
            nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        if(adaptive)     adaptiveProcedure(jump)
    },
    
    methods = list(
        
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
                setSize(scaleHistory,          timesAdapted)
                setSize(acceptanceRateHistory, timesAdapted)
                scaleHistory[timesAdapted] <<- scale
                acceptanceRateHistory[timesAdapted] <<- acceptanceRate
                gamma1 <<- 1/((timesAdapted + 3)^0.8)
                gamma2 <- 10 * gamma1
                adaptFactor <- exp(gamma2 * (acceptanceRate - optimalAR))
                scale <<- scale * adaptFactor
                timesRan <<- 0
                timesAccepted <<- 0
            }
        },
        
        reset = function() {
            scale <<- scaleOriginal
            timesRan      <<- 0
            timesAccepted <<- 0
            timesAdapted  <<- 0
            scaleHistory          <<- scaleHistory          * 0
            acceptanceRateHistory <<- acceptanceRateHistory * 0
            gamma1 <<- 0
        }
    ), where = getLoadingNamespace()
)


## ---- blocker------------------------------------------------------------
model <- readBUGSmodel('blocker', dir = system.file('classic-bugs',
      'vol1','blocker', package = 'nimble'))
model$tau
model$tau <- 0.01
conf <- configureMCMC(model)
conf$removeSamplers('tau')
# as baseline, use standard Metropolis for tau
conf$addSampler('tau', type = 'RW')
mcmc <- buildMCMC(conf)
niter <- 25000
Cmodel <- compileNimble(model)
cmcmc <- compileNimble(mcmc, project = model)
set.seed(0)
cmcmc$run(niter)
smp1 <- as.matrix(cmcmc$mvSamples)

## ---- scopefix, echo=FALSE-----------------------------------------------
# not clear why sampler_RW_reflect() not being put into global
# if this isn't done, configureMCMC fails to find sampler_RW_reflect in knitr
assign('sampler_RW_reflect', sampler_RW_reflect, .GlobalEnv)

## ---- add-reflect--------------------------------------------------------
conf$removeSamplers('tau')
# for comparison, consider the reflection sampler
conf$addSampler('tau', type = 'RW_reflect')
mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = model, resetFunctions = TRUE)

nimCopy(model, Cmodel)
set.seed(0)
cmcmc$run(niter)
smp2 <- as.matrix(cmcmc$mvSamples)

nplot <- 300
burnin <- 1000
plot(seq_len(nplot), smp1[seq_len(nplot), 'tau'], type = 'l', 
                     xlab = 'iteration', ylab=expression(tau))
lines(seq_len(nplot), smp2[seq_len(nplot), 'tau'], col = 'red')
library(coda, quietly = TRUE)
effectiveSize(log(smp1[(burnin+1):niter , 'tau']))
effectiveSize(log(smp2[(burnin+1):niter , 'tau']))

