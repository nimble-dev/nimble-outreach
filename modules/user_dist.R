## ----chunksetup, include=FALSE-------------------------------------------
# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)  # otherwise new() not being found - weird
library(nimble)

## ---- dmyexp-------------------------------------------------------------
dmyexp <- nimbleFunction(
    run = function(x = double(0), rate = double(0, default = 1), 
        log = integer(0, default = 0)) {
        returnType(double(0))
        logProb <- log(rate) - x*rate
        if(log) return(logProb)
        else return(exp(logProb)) 
    })

rmyexp <- nimbleFunction(
    run = function(n = integer(0), rate = double(0, default = 1)) {
        returnType(double(0))
        if(n != 1) print("rmyexp only allows n = 1; using n = 1.")
        dev <- runif(1)
        return(-log(1-dev) / rate)
    })

## ---- scopefix, echo=FALSE-----------------------------------------------
# not clear why dmyexp() not being put into global
# if this isn't done, registerDistributions fails to find dmyexp in knitr
assign('dmyexp', dmyexp, .GlobalEnv)
assign('rmyexp', rmyexp, .GlobalEnv)

## ---- register-dist------------------------------------------------------
registerDistributions(list(
        dmyexp = list(
               BUGSdist = "dmyexp(rate, scale)",
               Rdist = "dmyexp(rate = 1/scale)",
               altParams = c("scale = 1/rate", "mean = 1/rate"),
               pqAvail = FALSE,
               range = c(0, Inf)
               )))

## ---- use-dist, fig.cap=""-----------------------------------------------
code <- nimbleCode({
y ~ dmyexp(scale = mu)
mu ~ dunif(0, 10)
})
m <- nimbleModel(code, data = list(y = 1.2))
mcmcConf <- configureMCMC(m)
mcmcConf$getSamplers()
mcmc <- buildMCMC(mcmcConf)
niter <- 100
mcmc$run(niter)
plot(seq_len(niter), as.matrix(mcmc$mvSamples)[,1], type = 'l')

## ---- multi--------------------------------------------------------------
code <- nimbleCode({
for(i in 1:n) {
  # likelihood - word counts
  y[i, 1:K] ~ dmulti(p[i,1:K], N[i])
  # latent process (random effects) - topic model
  p[i, 1:K] ~ ddirch(alpha[topic[i], 1:K])
}
# prior for hyperparameters
for(tp in 1:M)
  for(k in 1:K)
  alpha[tp, k] ~ dunif(0, 100)
})

const <- list(M = 2, K = 4, n = 5, N = rep(1000, 5),
      topic = c(1, 1, 1, 2, 2))
alphaInits <- rbind(c(10, 30, 100, 3), c(12, 15, 15, 8))
m <- nimbleModel(code, constants = const, 
  inits = list(alpha = alphaInits))

## ---- multi-simulate-----------------------------------------------------
set.seed(0)
m$simulate(c('p', 'y'))
m$p
m$y

## ---- dirchmulti---------------------------------------------------------
ddirchmulti <- nimbleFunction(
            run = function(x = double(1), alpha = double(1), size = double(0),
                log = integer(0, default = 0)) {

                returnType(double(0))
                logProb <- lgamma(size) - sum(lgamma(x)) + 
                        lgamma(sum(alpha)) -
                        sum(lgamma(alpha)) + sum(lgamma(alpha + x)) - 
                        lgamma(sum(alpha) + size)
                if(log) return(logProb)
                        else return(exp(logProb))
})

rdirchmulti <- nimbleFunction(
            run = function(n = integer(0), alpha = double(1), 
                size = double(0)) {

                returnType(double(1))
                if(n != 1) print("rdirchmulti only allows n = 1; using n = 1.")
                p <- rdirch(1, alpha)
                return(rmulti(1, size = size, prob = p))
})

## ---- scopefix2, echo=FALSE----------------------------------------------
# not clear why ddirchmulti() not being put into global
# if this isn't done, registerDistributions fails to find ddirchmulti in knitr
assign('ddirchmulti', ddirchmulti, .GlobalEnv)
assign('rdirchmulti', rdirchmulti, .GlobalEnv)

## ---- register-dirchmulti------------------------------------------------
registerDistributions(list(
        ddirchmulti = list(
        BUGSdist = "ddirchmulti(alpha, size)",
        types = c('value = double(1)', 'alpha = double(1)'))
))

## ---- dirmulti-topic-----------------------------------------------------
code2 <- nimbleCode({
for(i in 1:n)
  # likelihood 
  y[i,1:K] ~ ddirchmulti(alpha[topic[i], 1:K], N[i])
# priors for hyperparameters
for(tp in 1:M)
  for(k in 1:K)
  alpha[tp, k] ~ dunif(0, 100)
})
m2 <- nimbleModel(code2, constants = const,
  inits = list(alpha = alphaInits))
set.seed(0)
m2$simulate('y')
m2$y

