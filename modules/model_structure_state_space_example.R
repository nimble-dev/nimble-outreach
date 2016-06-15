## ----loadnimble, include=FALSE-------------------------------------------
library(nimble)

## ----chunksetup, include=FALSE-------------------------------------------
# Following code is only needed for slide generation, not for using R code separately.
if(!('modules' %in% unlist(strsplit(getwd(), split = '/')))) setwd('modules')
library(methods)
read_chunk('chunks_ssm.R')

## ---- ssmSmall-code------------------------------------------------------

## ---- ssmSmall-model-----------------------------------------------------

## ------------------------------------------------------------------------
library(nimble)
library(igraph)
ssmSmallCode <- nimbleCode({
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

## @knitr ssmSmall-model
pyears <- 6 # Number of future years with predictions
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233, 209, 226, 192, 191, 225,
        245, 205, 191, 174, rep(NA, pyears))
year <- 1990:(2009 + pyears)

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))
## NIMBLE will handle y as data, T as a constant

ssmSmall <- nimbleModel(ssmSmallCode, constants = list(T = 5),
                        data = list(y = bugs.data$y[1:5]))

drawGraph <- function(model, colorBy = "none", colors = c('salmon','cyan','plum')) {
    graph <- model$getGraph()
    numNodes <- length(ssmSmall$getNodeNames())
    vertex.color <- rep('grey', numNodes)
    if(identical(colorBy, "none")) {}
    else if(length(colorBy) > 1) {
        if(is.character(colorBy))
            colorBy <- model$expandNodeNames(colorBy, returnType = "ids")
        vertex.color[colorBy] <- colors[1]
    } else if(colorBy == "stochDetermData") {
        stochIDs <- ssmSmall$getNodeNames(stochOnly = TRUE, returnType = "ids")
        determIDs <- ssmSmall$getNodeNames(determOnly = TRUE, returnType = "ids")
        dataIDs <- ssmSmall$getNodeNames(dataOnly = TRUE, returnType = "ids")
        vertex.color[stochIDs] <- colors[1]
        vertex.color[dataIDs] <- colors[2]
        vertex.color[determIDs] <- colors[3]
    } else if(colorBy == "topLatentEnd") {
        topIDs <- ssmSmall$getNodeNames(topOnly=TRUE, returnType = "ids")
        latentIDs <- ssmSmall$getNodeNames(latentOnly=TRUE, returnType = "ids")
        endIDs <- ssmSmall$getNodeNames(endOnly=TRUE, returnType = "ids")
        vertex.color[topIDs] <- colors[1]
        vertex.color[latentIDs] <- colors[2]
        vertex.color[endIDs] <- colors[3]
    }
    plot(graph, layout = layout_(graph, with_kk()),
         vertex.color = vertex.color ) ## uses plot.igraph
}

## ------------------------------------------------------------------------
drawGraph(ssmSmall, ssmSmall$getDependencies('r[3]'))

## ------------------------------------------------------------------------
ssmSmall$getDependencies('r[3]')

## ---- echo=FALSE---------------------------------------------------------
drawGraph(ssmSmall, ssmSmall$getDependencies('r[3]'))

## ------------------------------------------------------------------------
ssmSmall$getDependencies('r[3]', determOnly = TRUE)

## ---- echo=FALSE---------------------------------------------------------
drawGraph(ssmSmall, ssmSmall$getDependencies('r[3]', determOnly = TRUE))

## ------------------------------------------------------------------------
args(ssmSmall$getDependencies)
## or help(modelBaseClass)

## ------------------------------------------------------------------------
ssmSmall$getNodeNames()

## ------------------------------------------------------------------------
ssmSmall$getNodeNames(stochOnly = TRUE)   #salmon
ssmSmall$getNodeNames(determOnly = TRUE)  #cyan
ssmSmall$getNodeNames(dataOnly = TRUE)    #plum

## ------------------------------------------------------------------------
drawGraph(ssmSmall, 'stochDetermData')

## ------------------------------------------------------------------------
ssmSmall$getNodeNames(topOnly = TRUE)   #salmon
ssmSmall$getNodeNames(latentOnly = TRUE)  #cyan
ssmSmall$getNodeNames(endOnly = TRUE)    #plum

## ------------------------------------------------------------------------
drawGraph(ssmSmall, 'topLatentEnd')

## ------------------------------------------------------------------------
ssmSmall$expandNodeNames('logN.est[2:5]')

## ------------------------------------------------------------------------
ssmSmall$topologicallySortNodes(c('logN.est[5]', 'r[4]', 'logN.est[3]', 'y[2]'))

## ------------------------------------------------------------------------
m1 <- nimbleModel(
    nimbleCode({
        tau ~ dunif(0, 100)
        x ~ dnorm(0, tau) #by default, tau is a precision
    }))
plot(m1$getGraph())
m1$getNodeNames()

## ---- eval = FALSE-------------------------------------------------------
## nimbleCode({
##     tau ~ dunif(0, 100)
##     lifted_d1_over_sqrt_oPtau_cP <- 1/sqrt(tau)
##     x ~ dnorm(0, sd = lifted_d1_over_sqrt_oPtau_cP) #by default, tau is a precision
## }))

## ---- eval = FALSE-------------------------------------------------------
## m1$calculate(c('tau','x'))

## ---- eval = FALSE-------------------------------------------------------
## m1$calculate( m1$getDependencies('tau') )

## ------------------------------------------------------------------------
m2 <- nimbleModel(
    nimbleCode({
        a ~ dnorm(0, 1)
        b ~ dnorm(a + 1, 1)
    }))
plot(m2$getGraph())
m2$getNodeNames()

## ------------------------------------------------------------------------
ssmSmall$getVarNames()
ssmSmall$getVarInfo('r')

