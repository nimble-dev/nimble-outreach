## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)

## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ---- eval=FALSE---------------------------------------------------------
## x ~ dnorm(0, .01)

## ---- eval = FALSE-------------------------------------------------------
## x ~ dnorm(0, sd = 10)

## ---- eval = FALSE-------------------------------------------------------
## for(i in 1:10) {
##   logit(p[i]) <- intercept + slope * x[i]
## }

## ---- eval = FALSE-------------------------------------------------------
##    logit(p[1:10]) <- intercept + slope * x[1:10]

## ---- eval=FALSE---------------------------------------------------------
## regressionCode <- nimbleCode({
##     intercept ~ dnorm(0, sd = 1000)
##     slope1 ~ dnorm(0, sd = 1000)
##     if(includeX2) {  ## includeX2 could be any R expressions
##     ## This clause has code for including x2
##         slope2 ~ dnorm(0, sd = 1000)
##         for(i in 1:N)
##             predictedY[i] <- intercept + slope1 * x1[i] + slope2 * x2[i]
##     } else {
##     ## This clause has code for omitting x2
##         for(i in 1:N) predictedY[i] <- intercept + slope1 * x1[i]
##     }
##     sigmaY ~ dunif(0, 100)
##     for(i in 1:N) Y[i] ~ dnorm(predictedY[i], sigmaY)
## })
## 
## includeX2 <- FALSE ## includeX2 is defined in the R global environment
## modelWithoutX2 <- nimbleModel(regressionCode, constants = list(N = 30),
##                               check=FALSE)
## 
## includeX2 <- TRUE
## modelWithX2 <- nimbleModel(regressionCode, constants = list(N = 30),
##                            check = FALSE)

