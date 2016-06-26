## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)

## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ------------------------------------------------------------------------

scrCode <- nimbleCode({
    sigma~dunif(0, 15)
    lam0~dgamma(.1,.1)
    psi~dunif(0, 1)

    for (i in 1:M){	
        z[i]~dbern(psi)
	SX[i]~dunif(xl, xu)
	SY[i]~dunif(yl, yu)

        for(j in 1:ntraps) {
            D2[i,j] <- pow(SX[i]-trapmat[j,1], 2) + pow(SY[i]-trapmat[j,2],2)
            Eo[i,j] <- lam0*exp(-D2[i,j]/sigma)
            pmean[i,j]<-1-(exp(-Eo[i,j]))
            tmp[i,j]<-pmean[i,j]*z[i]
            y[i,j]~dbin(tmp[i,j],J)
        }
    }
    N<-sum(z[1:M])
})	

	

## ------------------------------------------------------------------------
scrCode2 <- nimbleCode({
    sigma~dunif(0, 15)
    lam0~dgamma(.1,.1)
    psi~dunif(0, 1)

    for (i in 1:M){	
        z[i]~dbern(psi)
	SX[i]~dunif(xl, xu)
	SY[i]~dunif(yl, yu)
        D2[i,1:ntraps] <- pow(SX[i]-trapmat[1:ntraps,1], 2) + pow(SY[i]-trapmat[1:ntraps,2],2)
        Eo[i,1:ntraps] <- lam0*exp(-D2[i,1:ntraps]/sigma)
        pmean[i,1:ntraps]<-1-(exp(-Eo[i,1:ntraps]))
        tmp[i,1:ntraps]<-pmean[i,1:ntraps]*z[i]
        for(j in 1:ntraps) {
            y[i,j]~dbin(tmp[i,j],J)
        }
    }
    N<-sum(z[1:M])
})	

## will run below:
## scrModel2 <- nimbleModel(scrCode2, constants = list(M = M, ntraps = ntraps, J = J,
##                                                   xl = xl, yl = yl,
##                                                   xu = xu, yu = yu),
##                                  data = list(trapmat = trapmat), check = FALSE) 

## ------------------------------------------------------------------------
## From Royle & Dorazio:
sigma=3
## beta=1 ## beta is never used so must have been a typo in book code
lam0=.6
J=10          # number of capture periods
llx=0
upx=18
xl=llx
yl=llx
yu=upx
xu=upx

#Create a 10x10 grid of traps

x=4:13
y=4:13
locs=expand.grid(x,y)
ntraps=dim(locs)[1] ## thats 10x10 = 100 cameras
trapmat=as.matrix(locs)

## Use NIMBLE:
M <- 128
scrModel2 <- nimbleModel(scrCode2, constants = list(M = M, ntraps = ntraps, J = J,
                                                  xl = xl, yl = yl,
                                                  xu = xu, yu = yu),
                                 data = list(trapmat = trapmat), check = FALSE) 
scrModel2$sigma <- sigma
scrModel2$lam0 <- lam0
scrModel2$z <- c(rep(1, 28), rep(0, 100)) ## R&D simulate 28 animals.
set.seed(123)
scrModel2$simulate( scrModel2$getDependencies(c('SX','SY'), downstream = TRUE))
dim(scrModel2$y)
scrModel2$y[1:3,] ## real animals
scrModel2$y[29:31,] ## zeros in augmentation range

## ----fig.cap = NULL------------------------------------------------------
plot(c(llx,upx), c(llx, upx), typ='n')
points(locs)
points(scrModel2$SX[1:28], scrModel2$SY[1:28], col = 'red')

## ------------------------------------------------------------------------
## We'll manually set initial values similarly to R&D
## This MCMC can fail if the initial values provide -Inf likelihood terms
scrModel2$sigma <- 5
scrModel2$lam0 <- lam0
scrModel2$psi <- 0.6
scrModel2$SX <- initSX <- runif(M, 4, 13) ## R&D use runif(M, 2, 10) for reasons I don't see
scrModel2$SY <- initSY <- runif(M, 4, 13) ## ditto
scrModel2$z <- rbinom(M, 1, .5)
## not fully necessary, but it seems cleaner to set animals that were seen
## to definitely exist
scrModel2$z[ rowSums(scrModel2$y) > 0 ] <- 1 
initZ <- scrModel2$z
scrModel2$setData(list(y = scrModel2$y)) 

## ------------------------------------------------------------------------
defaultMCMCconf <- configureMCMC(scrModel2, monitors = c('sigma','lam0','psi','N'))
## For illustration, let's look at samplers for a few nodes of each variable
defaultMCMCconf$printSamplers(c('sigma', 'lam0','psi','SX[1:3]', 'SY[1:3]', 'z[1:3]'))

## ------------------------------------------------------------------------
sampler_scr_locations <- nimbleFunction(
    contains = sampler_BASE, ## Class inheritance system
    setup = function(model, mvSaved, target, control) { ## required args
        ## target will be a vector like c('SX[3]','SY[3]')
        indicatorNode <- control$indicatorNode ## like 'z[3]'
        calcNodes <- model$getDependencies(target)
        ## The rest of this can be ignored: It is boilerplate for proposal scale adaptation
        scaleOriginal <- 1
        scale <- 1
        adaptive <- TRUE
        timesRan <- 0
        timesAccepted <- 0
        timesAdapted <- 0
        optimalAR <- 0.44 ## Actually I've hastily thrown in the 1D case instead of 2D
        gamma1    <- 0
        adaptInterval <- 100
    },    
    run = function() {
        if(model[[indicatorNode]]==0) return() ## Locations will be sampled elsewhere
        location <- values(model, target)
 
        location[1] <- rnorm(1, mean = location[1], sd = scale)
        location[2] <- rnorm(1, mean = location[2], sd = scale)

        values(model, target) <<- location

        logMHR <- calculateDiff(model, calcNodes)
        accept <- decide(logMHR)
        if(accept)
            nimCopy(from = model, to = mvSaved, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        else
            nimCopy(from = mvSaved, to = model, row = 1, nodes = calcNodes, 
                         logProb = TRUE)
        if(adaptive) adaptiveProcedure(accept)
    },
    methods = list(
        ## from adaptive MCMC theory, copied from our RW sampler source code
        ## Should be modified since it is really a 2D sampler, but this will do for now
        adaptiveProcedure = function(jump = logical()) {
            timesRan <<- timesRan + 1
            if(jump)     timesAccepted <<- timesAccepted + 1
            if(timesRan %% adaptInterval == 0) {
                acceptanceRate <- timesAccepted / timesRan
                timesAdapted <<- timesAdapted + 1
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
            gamma1 <<- 0
        })
)

## ------------------------------------------------------------------------
customMCMCconf <- configureMCMC(scrModel2, nodes = c('lam0','sigma','psi'), monitors = c('lam0','sigma','psi','N'))
customMCMCconf$printSamplers()
zNodes <- scrModel2$expandNodeNames('z')
SXnodes <- scrModel2$expandNodeNames('SX')
SYnodes <- scrModel2$expandNodeNames('SY')
## Illustrate what these vectors of node names look like
zNodes[1:3]
SXnodes[1:3]
SYnodes[1:3]
## add the samplers
for(i in 1:length(zNodes))
    customMCMCconf$addSampler(sampler_scr_locations,
                              target = c(SXnodes[i], SYnodes[i]),
                              control = list(indicatorNode = zNodes[i]))
## Illustrate that they have been added
customMCMCconf$printSamplers('SX[1:3]')

## ------------------------------------------------------------------------
sampler_scr_inclusion <- nimbleFunction(
    contains = sampler_BASE, ## Class inheritance system
    setup = function(model, mvSaved, target, control) { ## required args
        ## target will be a vector like c('SX[3]','SY[3]')
        indicatorNode <- control$indicatorNode ## like 'z[3]'
        calcNodes <- model$getDependencies(c(target, indicatorNode))
        indicatorZeroCalcs <- model$getDependencies(indicatorNode)
        probDoSampler <- 0.2 # don't sample every indicator on every iteration
    },
    run = function() {
        if(runif(1,0,1) > probDoSampler) return()
        if(model[[indicatorNode]]==0) {
            ## propose putting in model
            currentLogProb <- model$getLogProb(indicatorZeroCalcs)
            model[[indicatorNode]] <<- 1
            model$simulate( target )
            proposalLogProb <- model$calculate(calcNodes)
            logProbForwardProposal <- model$getLogProb(target) ## proposal prob
            log_accept_prob <- proposalLogProb - currentLogProb - logProbForwardProposal
        } else {
            ## propose removing from model
            currentLogProb <- model$getLogProb(calcNodes)
            model[[indicatorNode]] <<- 0
            proposalLogProb <- model$calculate(indicatorZeroCalcs)
            logProbReverseProposal <- model$calculate(target)
            log_accept_prob <- proposalLogProb - currentLogProb + logProbReverseProposal
        }

        accept <- decide(log_accept_prob)
        ## There is a bit of excess copying here that could be reduced
        ## depending on which of the above cases was used
        ## but for simplicity I'm leaving it as is
        if(accept)
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


## ------------------------------------------------------------------------
for(i in 1:length(zNodes))
    customMCMCconf$addSampler(sampler_scr_inclusion,
                              target = c(SXnodes[i], SYnodes[i]),
                              control = list(indicatorNode = zNodes[i]))
## Illustrate that they were added
## Note that the z[i]s are sampled even though
## not provided via the "target" argument
customMCMCconf$printSamplers('SX[1:3]')

## ------------------------------------------------------------------------
defaultMCMC <- buildMCMC(defaultMCMCconf)
customMCMC <- buildMCMC(customMCMCconf)
compiled <- compileNimble(scrModel2, defaultMCMC, customMCMC)

## ------------------------------------------------------------------------
timeDefaultMCMC <- system.time(compiled$defaultMCMC$run(10000))
## To be fair, we'll reset states
compiled$scrModel2$SX <- initSX
compiled$scrModel2$SX <- initSY
compiled$scrModel2$SX <- initZ
compiled$scrModel2$sigma <- 5
compiled$scrModel2$lam0 <- lam0
compiled$scrModel2$psi <- 0.6
timeCustomMCMC <- system.time(compiled$customMCMC$run(10000))

## ------------------------------------------------------------------------
i <- 1000:10000
## inspection of results shows they are similar
library(coda)
## How do the effective sample sizes compare?
effectiveSize(as.matrix(compiled$defaultMCMC$mvSamples)[i,])
effectiveSize(as.matrix(compiled$customMCMC$mvSamples)[i,])
## How do the times compare?
timeDefaultMCMC
timeCustomMCMC
## How does MCMC efficiency compare?
effectiveSize(as.matrix(compiled$defaultMCMC$mvSamples)[i,])/timeDefaultMCMC[3]
effectiveSize(as.matrix(compiled$customMCMC$mvSamples)[i,])/timeCustomMCMC[3]

