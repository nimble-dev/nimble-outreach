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

## ----node-names----------------------------------------------------------
ssm$getNodeNames()

## ---- model-values-------------------------------------------------------
ssm$mean.r
ssm$logProb_mean.r  # log-likelihood components
ssm$r
ssm$r <- rnorm(length(ssm$r), 0, 1)
ssm$r
ssm$logProb_r # not calculated yet!

## ---- model-operate------------------------------------------------------
ssm$r[1:3] <- c(.1, .2, .3)
ssm$calculate(c('r[1]','r[2]','r[3]'))
## The following is equivalet:
## ssm$calculate('r[1:3]')
## If we wanted to calculate all of r, we could do:
## ssm$calculate('r')
ssm$logProb_r[1:3]
sum(ssm$logProb_r[1:3])

## ---- model-simulate-----------------------------------------------------
set.seed(0)  # so the calculations are reproducible
ssm$simulate('r[1:3]')  # simulate from prior
ssm$r[1:3]

## ------------------------------------------------------------------------
ssm$getParam('r[1]', 'sd')

## ------------------------------------------------------------------------
ssm$getParam('r[1]', 'tau') # label for precision

## ------------------------------------------------------------------------
ssm$getLogProb('r[1:3]')

## ------------------------------------------------------------------------
values(ssm, c('r[1:3]','logN.est[1:3]'))
values(ssm, c('r[1:3]','logN.est[1:3]')) <- 1:6
ssm$r[1:3]
ssm$logN.est[1:3]

## ------------------------------------------------------------------------
set.seed(0)
Cssm$r[1:3]
Cssm$simulate('r[1:3]')
Cssm$calculate('r[1:3]')

## ------------------------------------------------------------------------
## Put new values in r[1:3]
ssm$r[1:3] <- c(0.1, 0.2, 0.3)
## need to update log probabilities of r[1:3], values of logN.est[2:T], and log probabilities of logN[2:T], in that order.
## If we go out of order, calculates will not be correct.
ssm$calculate(c('r[1:3]','logN.est[2:T]', 'y[2:T]'))

