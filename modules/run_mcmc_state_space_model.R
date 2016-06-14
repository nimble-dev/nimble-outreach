## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)
read_chunk('chunks_ssm.R')

## ---- ssm-code-----------------------------------------------------------

## ---- ssm-model----------------------------------------------------------

## ---- ssm-compile--------------------------------------------------------

## ---- conf---------------------------------------------------------------
mcmcConf <- configureMCMC(ssm)

## ----seesamplers---------------------------------------------------------
mcmcConf$printSamplers()

## ---- seemonitors--------------------------------------------------------
mcmcConf$getMonitors()

## ----build-mcmc----------------------------------------------------------
ssmMCMC <- buildMCMC(mcmcConf)
CssmMCMC <- compileNimble(ssmMCMC, project = ssm)

## ----run-mcmc, warning=FALSE---------------------------------------------
set.seed(0)
print(system.time(ssmMCMC$run(10)))  # uncompiled version
set.seed(0)
print(system.time(CssmMCMC$run(10)))   # compiled version

## ----run-mcmc-longer-----------------------------------------------------
print(system.time(CssmMCMC$run(10000)))

## ----Rmcmc---------------------------------------------------------------
samples <- as.matrix(CssmMCMC$mvSamples)

## ----output-mcmc, fig.height=4, fig.width=9, fig.cap=""------------------
tsplot <- function(x, ...) plot(seq_along(x), x, type = 'l', ...)

par(mfrow = c(1, 3), mai = c(.6, .5, .1, .2))
tsplot(samples[ , 'mean.r'], xlab = 'iteration',
     ylab = 'mean.r', main = 'mean.r')
tsplot(samples[ , 'sigma.proc'], xlab = 'iteration',
     ylab = 'sigma.proc', main = 'sigma.proc')
tsplot(samples[ , 'sigma.obs'], xlab = 'iteration',
     ylab = 'sigma.obs', main = 'sigma.obs')

## ----coda----------------------------------------------------------------
library(coda)
burnin <- 100
mcmc <- as.mcmc(samples[(burnin+1):nrow(samples), ])
crosscorr(mcmc[ , c('mean.r', 'sigma.proc', 'sigma.obs')])
effectiveSize(mcmc)

