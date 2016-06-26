## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)

## ---- nf-----------------------------------------------------------------
objective <- nimbleFunction(
    setup = function(model, target) {
          calcNodes <- model$getDependencies(target)
          },
    run = function(par = double(1)) {
        returnType(double(0))
        values(model, target) <<- par
        ans <- model$calculate(calcNodes)
        return(ans)
    }
)

## ---- model--------------------------------------------------------------
code <- nimbleCode({
     for(i in 1:n) {
           y[i] ~ dbern(p[i])
           p[i] <- expit(b0 + b1*x[i])
     }
})
n <- 300
b0 <- -0.5
b1 <- 4
x <- runif(n, -1, 1)
p <- expit(b0 + b1*x)
y <- rbinom(n, 1, p)

model <- nimbleModel(code, constants = list(n = n, x = x),
      data = list(y = y), inits = list(b0 = 0, b1 =0))
Cmodel <- compileNimble(model)
logistic_obj <- objective(model, c('b0', 'b1'))
Clogistic_obj <- compileNimble(logistic_obj, project = model)

## ---- optimizer----------------------------------------------------------
out <- optim(c(Cmodel$b0, Cmodel$b1), Clogistic_obj$run, 
    control = list(fnscale = -1))
out$par
glm(y ~ x, family = 'binomial')

## ------------------------------------------------------------------------
simMany <- nimbleFunction(
    setup = function(model, parameters) {
          calcNodes <- model$getDependencies(parameters, self = FALSE)
          results <- modelValues(model)
    },
    run = function(p = double(1), m = integer()) {
       values(model, parameters) <<- p
       resize(results, m)
       for(i in 1:m) {
         model$simulate(calcNodes, includeData = TRUE)
         copy(model, results, nodes = calcNodes,
              logProb = FALSE, row = i)
       }
    }
)

## ------------------------------------------------------------------------
## Make a specialized case:
simManyGLM <- simMany(model = model, c('b0','b1'))
set.seed(1)
simManyGLM$run(c(.2, .8), 10)
## Look at first 20 y's to keep this small
as.matrix(simManyGLM$results)[, model$expandNodeNames('y')[1:20]]

## ------------------------------------------------------------------------
CsimManyGLM <- compileNimble(simManyGLM, project = model)
set.seed(1)
CsimManyGLM$run(c(.2, .8), 10)
## Look at first 20 y's to keep this small
as.matrix(CsimManyGLM$results)[, model$expandNodeNames('y')[1:20]]

