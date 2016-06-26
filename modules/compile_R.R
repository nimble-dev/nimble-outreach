## ----chunksetup, include=FALSE-------------------------------------------
# include any code here you don't want to show up in the document,
# e.g. package and dataset loading
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)  # otherwise new() not being found - weird
library(nimble)

## ---- rcFun1-------------------------------------------------------------
nimExp <- nimbleFunction(
       # run-time code for our computation
       run = function(x = double(1)) {
           returnType(double(1))
           n <- length(x)
           # some functions, like numeric, mimic R
	   # but also may have additional/different features
           out <- numeric(n, init = FALSE)
           # core computation
           for( i in 1:n) 
                out[i] <- exp(x[i])
           return(out)
})

## ---- rcFun2-------------------------------------------------------------
nimExp2 <- nimbleFunction(
       run = function(x = double(1)) {
           returnType(double(1))
           out <- exp(x)
           return(out)
})

## ---- compareRcSpeed-----------------------------------------------------
cnimExp <- compileNimble(nimExp)
cnimExp2 <- compileNimble(nimExp2)

x <- rnorm(1e6)
library(rbenchmark)
benchmark(out0 <- exp(x),
               out1 <- cnimExp(x),
               out2 <- cnimExp2(x),
               columns = c('test','replications','elapsed'),
               replications = 10)

## ---- probit-------------------------------------------------------------
set.seed(0)
M <- 1000000
system.time({
        alphas <- c(-3, -0.5, -0.25, .1, .15, .29, .4, .45)
        K <- length(alphas)
        # generate W_k ~ N(alpha_k, 1)
        rands <- matrix(rnorm(M*K), nrow = K, ncol = M)
        props <- rep(0, K)
        tmp <- alphas + rands # exploit vectorization
        # now tally the results
        id <- apply(tmp, 2, which.max)
        tbl <- table(id)
        props[as.integer(names(tbl))] <- tbl / M
})

mprobit <- nimbleFunction(
         run = function(alphas = double(1), M = double(0)) {
             returnType(double(1))
             K <- length(alphas)
	     props <- numeric(K) # initialized to 0 by default
	     w <- numeric(K, init = FALSE)
             for(m in 1:M) {
                   for(k in 1:K) 
                        w[k] <- alphas[k] + rnorm(1) 
                   maxind <- K
                   max <- w[K]
                   for(k in 1:(K-1)) {
                        if(w[k] > max){
                                maxind <- k
                                max <- w[k]          
                        }
                   }
                   props[maxind] <- props[maxind] + 1
             }
	     props <- props/M
             return(props)
         }
)

cmprobit = compileNimble(mprobit)
set.seed(0)
system.time(
props2 <- cmprobit(alphas, M)
)

