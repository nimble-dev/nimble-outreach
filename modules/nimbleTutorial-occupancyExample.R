## Load the package.
## See User Manual ch. 4 for installation instructions
library(nimble)
set.seed(12345) ## so we all get the same random numbers

## We can load the classic WinBUGS examples directly,
## but it is more instructive to enter it directly here.

## We can read code from a file or enter it via the
## nimbleCode function
pumpCode <- nimbleCode({ 
  for (i in 1:N){
      theta[i] ~ dgamma(alpha,beta)
      lambda[i] <- theta[i]*t[i]
      x[i] ~ dpois(lambda[i])
  }
  alpha ~ dexp(1.0)
  beta ~ dgamma(0.1,1.0)
})

## NIMBLE makes a distinction between constants and data
## that other packages don't.  Constants cannot be changed
## after the model is defined by the nimbleModel function.
## Data are values for nodes in a model that will be marked as data.
## Algorithms may  need to know which nodes are marked as data, but
## the actual values may be changed.

## N must be provided as a constant, but t could be provided
## either as a constant or as data (or neither).
pumpConsts <- list(N = 10,
                   t = c(94.3, 15.7, 62.9, 126, 5.24,
                       31.4, 1.05, 1.05, 2.1, 10.5))

pumpData <- list(x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22))

## Initial values are not necessary but handy to provide now
pumpInits <- list(alpha = 1, beta = 1,
                  theta = rep(0.1, pumpConsts$N))

## Create the model object.  
## The only arguments that must be provided now are code and constants.
## Others can be provided later.
pump <- nimbleModel(code = pumpCode, name = 'pump', constants = pumpConsts,
                    data = pumpData, inits = pumpInits)
## We refer to pump informally as the "R model" or "uncompiled model".

## Look at what we can do with the model object
## We can access and change values
pump$alpha
pump$alpha <- 1.5
pump$alpha
pump$beta
pump$x
pump$lambda
pump$theta
## Note that there is no pump$t since it was provided as constants, so the following won't work:
pump$t
## User-friendly error messages, anyone?
## verify that lambda[i] == theta[i] * t[i]
pump$theta * pumpConsts$t
## We can also access values using double-bracket notation
pump[['alpha']]
## Just as for R lists, this is useful because we can have a variable with the name
theNodeIwant <- 'alpha'
pump[[theNodeIwant]]

## We can operate the model using calculate, simulate, getLogProb, or calcuateDiff
## calculate returns the log probability of a stochastic variable
calculate(pump, 'x[1]') ## this is the log poisson probability of x[1]
## verify the result
dpois(pump$x[1], pump$lambda[1], log = TRUE)
## For a deterministic variable, calculate updates the value and returns 0
## look at current values of 
pump$theta[2]
pump$lambda[2]
## change value of lambda[2] and recalculate value of theta
pump$theta[2] <- 0.5
calculate(pump, 'lambda[2]')
pump$lambda[2]
## verify the result
pump$theta[2] * pumpConsts$t[2]

## getLogProb obtains the most recently calculated value of any log probability
calculate(pump, 'x[2]') ## this actually calculates the value and returns it
getLogProb(pump, 'x[2]') ## this just returns the most recent value, without re-calculating it

## calculateDiff calculates the log probability value and returns the difference between it and the previous value
## this is useful for MCMC and some weighting algorithms
previousLogProb <- getLogProb(pump, 'x[2]')
pump$x[2] <- pump$x[2] + 1
calculateDiff(pump, 'x[2]')
## verify the result
getLogProb(pump, 'x[2]') - previousLogProb

## simulate generates new random values into stochastic nodes
pump$theta[3]
simulate(pump, 'theta[3]')
pump$theta[3]
## note that the log probability is not updated until we call calculate
calculate(pump, 'theta[3]')
## For a deterministic node, simulate is like calculate (but doesn't return anything)
pump$lambda[3] ## This has not yet been updated since change the value of theat[3]
simulate(pump, 'lambda[3]')  ## We could use calculate instead
pump$lambda[3] ## now it is updated
## verify the result
pump$theta[3] * pumpConsts$t[3]

## simulate will not over-ride data values unless we explicitly tell it to
saved_x <- pump$x
pump$x[4]
simulate(pump, 'x[4]')
pump$x[4] ## nothing changed
## use includedData = TRUE to say nodes marked as data should be included in any simulation
simulate(pump, 'x[4]', includeData = TRUE) 
pump$x[4] ## now it was changed
## let's return it to its original value before moving on
pump$x <- saved_x
pump$x[4]

## We can use calculate, simulate, getLogProb, or calculateDiff on groups of nodes.
## When we do so, they will always be called in valid order, which is called
## "topologically sorted" order.
cat(pump$theta[5], pump$lambda[5], pump$x[5], '\n')
simulate(pump, c('theta[5]','lambda[5]', 'x[5]'), includeData = TRUE)
cat(pump$theta[5], pump$lambda[5], pump$x[5], '\n')
## verify that lambda[5] is correct
pump$theta[5] * pumpConsts$t[5]
## Let's demonstrate that the simulation order is always valid.
## Simulating lambda[5] before theta[5] would be incorrect, because
## lambda[5] must use the new value of theta[5].
simulate(pump, c('lambda[5]', 'theta[5]', 'x[5]'), includeData = TRUE)
cat(pump$theta[5], pump$lambda[5], pump$x[5], '\n')
## verify that lambda[5] is correct
pump$theta[5] * pumpConsts$t[5]
## This means the simulation was done in correct order

## If we don't provide a set of nodes for calculate, simulate, getLogProb, or calculateDiff,
## the default is to use the whole model
calculate(pump) ## this is the sum of log probability values for the whole model

## let's reset the data to original value(s)
pump$x <- saved_x

## Another way to use groups of nodes is with standard R notation for index sequences:
pump$theta
pump$lambda
simulate(pump, c('theta[1:5]', 'lambda[1:5]'))
pump$theta
pump$lambda
## The first five values of each of theta and lambda have been changed
## When we use calculate with groups of nodes, each of the log probability values is
## stored, and their sum is returned
calculate(pump, c('theta[1:5]', 'lambda[1:5]', 'x[1:5]'))
## getLobProb returns the sum of stored log probability values
getLogProb(pump, c('theta[1:5]', 'lambda[1:5]', 'x[1:5]'))
## calculateDiff returns the sum of log probability differences
## If we call it now, it will return 0 since we haven't changed any of the node values
calculateDiff(pump, c('theta[1:5]', 'lambda[1:5]', 'x[1:5]'))

## Internally, the log probability values are always stored like this:
pump$logProb_theta
## which can be useful for inspection, but the proper way to access these values in a program
## is via getLogProb.

## At this point we have seen that we can operate models by accessing and modifying their values
## and controlling calculations and simulations.  But so far we have needed to know what nodes
## are in the model, for example when we use names like 'theta[1:5]' above.  What we want is to
## to be able to learn about nodes and how they are related from the model object itself.

## Querying the model
## First we can see what nodes and variables are in the model
pump$getNodeNames()
pump$getVarNames()
## Notice there is a mysterious node called lifted_d1_over_beta
## This was inserted by NIMBLE to calculate 1/beta, which is needed as the scale
## parameter (1/rate) of the gamma distribution for the theta[i]s.  We call such nodes
## "lifted" because they have been pulled out of the declaration for the theta[i]s. The "d" in "d1"
## stands for "double precision" and is just a character to ensure a valid name in some cases.
## Otherwise the name of the node is semi-reading, such as "1_over_beta".

## We can get information about any of the variables:
pump$getVarInfo('x')
## This shows the number of dimensions (nDim), the range for each dimension, from mins (always 1), to maxs.
## In this case we see x has length of 10 because it has 1 dimension and the maximum index is 10.

## We can learn about the roles played by different nodes.
## To learn more about getNodeNames, we can look at its arguments
args(pump$getNodeNames)
## or the help page for the model class
help(modelBaseClass)
## Let's try some of the options
pump$getNodeNames(determOnly = TRUE) ## deterministic nodes
pump$getNodeNames(stochOnly = TRUE) ## stochastic nodes
pump$getNodeNames(stochOnly = TRUE, includeData = FALSE) ## stochastic nodes without data nodes
pump$getNodeNames(dataOnly = TRUE) ## data nodes
pump$getNodeNames(topOnly = TRUE) ## top-level nodes, typically parameters 
pump$getNodeNames(latentOnly = TRUE) ## latent nodes (nodes that are not data and are not top nodes)
pump$getNodeNames(latentOnly = TRUE, stochOnly = TRUE) ## stochastic latent nodes
pump$getNodeNames(includeRHSonly = TRUE) ## This has no impact for this example.  If t was not in constants, it would be a "right-hand-side only" node
pump$getNodeNames(endOnly = TRUE) ## For this example, end nodes are only the data nodes, but in other cases they can include posterior predictive nodes

## Every vector of nodes was returned in topologically sorted order.
## This means we can use them in calculate, simulate, getLogProb, and calculateDiff
## Let's get the sum of log probability values for the data nodes:
getLogProb(pump, pump$getNodeNames(dataOnly = TRUE))
## Notice something very important: We did not have to know anything about pump to do that.
## The same call would work no matter what is in the pump model.  This is the first step
## to model-generic programming.

## Next, we can get information about how nodes are connected in the model.
## Say we want to do MCMC on 'theta[5]' and we need to know which nodes depend on it.
## We call these the "stochastic dependencies"
## To keep it generic, we'll put the name 'theta[5]' in a variable
targetNode <- 'theta[5]'
pump$getDependencies(targetNode)
## We see that following a change to 'theta[5]', the nodes that will need
## (re-)calculation are 'theta[5]', 'lambda[5]', and 'x[5]'.
## By default, the dependencies include the targetNode itself, but this can
## be explicitly excluded (see below).
## This allows more model-generic programming.  For example
## if we modify targetNode, we can determine what needs recalculation
## more *any* model we are using:
## Let's suppose for illustration that we are simply going to add 0.1 to the target node
pump[[targetNode]] <- pump[[targetNode]] + 0.1
calculate(pump, pump$getDependencies(targetNode))

## Note that dependencies of one of the top-level parameters can be larger;
pump$getDependencies('alpha')

## We can also get dependencies of groups of nodes
pump$getDependencies('theta[6:10]')

## Now let's see some of the control we have over dependencies
## Again we can learn about the function by
args(pump$getDependencies)
## or
help(modelBaseClass)
## We can see that the arguments to control what we want from
## getDependencies are similar to those for getNodeNames, so
## we won't walk through them all.  But here are a couple of distinct
## ones to note:
## Use self = FALSE to omit the nodes provided to the function
pump$getDependencies(targetNode, self = FALSE)
## Use downstream = TRUE to trace dependencies through the entire graph
## rather than stopping at stochastic nodes
pump$getDependencies('alpha', downstream = TRUE)

## Writing simple programs to use models
## Let's say you are going to need to repeatedly simulate part of your model
## and from each simulation you want the sum of log probabilities returned
##
## If you were going to write a simple R function, it might look like this

simulateSomeNodes <- function(model, nodes) {
    dependencies <- model$getDependencies(nodes)
    simulate(model, dependencies)
    calculate(model, dependencies) ## last value in an R function is always returned
}

simulateSomeNodes(pump, 'theta[1:5]')

## That worked, but let's think more deeply about what we need.
## Often, algorithms for general models will be highly computational and may
## run for minutes, hours or days.  So we want to achieve efficiency where we can.
## The "model$getDependencies(nodes)" is redundant if we are going to call simulateSomeNodes
## repeatedly with the same arguments.

## NIMBLE provides a two-stage programming system using nimbleFunctions to do this.
## We can rewrite the above concept this way
simulateSomeNodesNF <- nimbleFunction(
    setup = function(model, nodes) {
        dependencies <- model$getDependencies(nodes)
    },
    run = function() {
        simulate(model, dependencies)
        ans <- calculate(model, dependencies)
        return(ans)
        returnType(double())
    }
    )

## Notice that the arguments to nimbleFunction include two function definitions,
## one called "setup" and one called "run"

## Now there are two steps to using this function.  Let's look first and then explain:
simulateSomeNodesNF_theta1to5 <- simulateSomeNodesNF(pump, 'theta[1:5]')
simulateSomeNodesNF_theta1to5$run()

## When we call simulateSomeNodesNF, the arguments go to the setup function, which is executed.
## Any objects created -- in this case "dependencies" -- are saved for later use.  The object returned
## simulateSomeNodesNF can be used to call the run function.  It uses the saved "dependencies" rather
## than creating it each time.  Now if we need to call it repeatedly, it will be more efficient
## than if we were determining dependencies each time.
simulateSomeNodesNF_theta1to5$run()
simulateSomeNodesNF_theta1to5$run()
simulateSomeNodesNF_theta1to5$run()

## Something else to notice is that we had to say what type of object will be returned  by the run
## function.  This is given by "returnType(double())".  In this case, "double()" refers to a double precision
## (numeric) scalar.  It is equivalent to say "double(0)", meaning a double with 0 dimensions.
## The return type is used by the compilation system, which we introduce next.

## Compiling the model and nimbleFunctions
## So far we have been using the uncompiled model and nimbleFunctions.  That means whenever we do something,
## the internal steps are being executed completely in R.  For many algorithms, execution in R is (much) too
## slow to make one happy.  So NIMBLE can generate C++ for the model and nimbleFunctions, compile the C++, load
## the compiled stuff, and let you use it in the same was as the uncompiled versions.

## We can compile the model and nimbleFunctions in one step or multiple steps.  Here we'll use one step
compiledPumpStuff <- compileNimble(pump, simulateSomeNodesNF_theta1to5)
cpump <- compiledPumpStuff$pump
csimulateSomeNodesNF_theta1to5 <- compiledPumpStuff$simulateSomeNodesNF_theta1to5
cpump$x
cpump$theta
csimulateSomeNodesNF_theta1to5$run()

## Let's see how much faster the compiled version is.  We'll run each version 100 times to
## get some appreciable times.
uncompiledTime <- system.time(for(i in 1:100) simulateSomeNodesNF_theta1to5$run())
compiledTime <- system.time(for(i in 1:100) csimulateSomeNodesNF_theta1to5$run())
uncompiledTime
compiledTime
## It's a little confusing what the three kinds of timing are, but we'll compare the last one
uncompiledTime[3] / compiledTime[3]
## on my system the compiled version is around 580 times faster.  That's about 1 second:9.7 minutes.

## Writing meaningful algorithms.
## Let's see what a very basic MCMC would look like as a nimbleFunction.
## We'll use a random walk Metropolis-Hastings sampler.  This makes proposals
## as a normally distributed "random walk" around the current value.
## We'll use the world's simplest model for this.
simpleCode <- nimbleCode({
    x ~ dnorm(0, 1)
    y ~ dnorm(x, 1)
})

simpleModel <- nimbleModel(code = simpleCode, data = list(y = 3.69))
## We can ignore the warning message. This just tells us the model
## isn't fully populated with meaningful values yet.

## We can initialize it this way
simulate(simpleModel)
calculate(simpleModel)

## Now let's write the simple Metropolis-Hastings
## We'll assume that each time it is called, the model
## has been completely calculated (all log probabilities are up to date).
## And we'll require that each time the run function finishes,
## it leaves the model completely calculated (it doesn't modify values
## and leave log probabilities out of date).
simpleMH <- nimbleFunction(
    setup = function(model, node, proposalSD) {
        calcNodes <- model$getDependencies(node)
    },
    run = function() {
        currentValue <- model[[node]]
        proposalValue <- rnorm(1, currentValue, proposalSD)
        model[[node]] <<- proposalValue
        logAcceptanceProbability <- calculateDiff(model, calcNodes)
        if(runif(1,0,1) < exp(logAcceptanceProbability)) accept <- TRUE else accept <- FALSE
        if(!accept) {
            model[[node]] <<- currentValue
            calculate(model, calcNodes)
        }
        return()
        returnType(void())
    })

## Now let's use it for x
## I am deliberately using a proposal standard deviation (proposalSD) that will yield poor mixing
simpleMHx <- simpleMH(simpleModel, 'x', proposalSD = 0.1)
simpleMHx$run()
simpleMHx$run()

## Now let's collect a sample and time it
xPosterior <- numeric(100)
uncompiledTime <- system.time(
    for(i in 1:100) {
        simpleMHx$run()
        xPosterior[i] <- simpleModel$x
    }
)
plot(xPosterior, type = 'b')
uncompiledTime

## Now that didn't look like it was mixing well, and we could try a different proposal standard deviation (proposalSD).
## In the "real" version of a random walk MH algorithm, the proposal SD adapts to a good value as the sampler is called repeatedly.
simpleMHx2 <- simpleMH(simpleModel, 'x', proposalSD = 0.8)
xPosterior2 <- numeric(100)
uncompiledTime2 <- system.time(
    for(i in 1:100) {
        simpleMHx2$run()
        xPosterior2[i] <- simpleModel$x
    }
)
plot(xPosterior2, type = 'b')
uncompiledTime2
## That's better mixing.  We can see sometimes big jumps are made, and sometimes a proposal is rejected, so x
## stays at the same value for multiple steps.
## But the time is pretty similar.

## Now we can do better writing this sampler in a couple of ways.  First, we have a utility function called decide()
## that determines whether we will accept or reject, instead of the line with runif(1,0,1)
## Second, we want to avoid recalculating the model every time we reject a proposal.
## A way to do this is to use a modelValues object.  This is an object that can store multiple sets of values for
## model variables and their associated log probabilities.  NIMBLE provides a special nimCopy function to move values
## between modelValues and model objects. ("copy" is an alias for "nimCopy" in a run function).
## Third, we want a better way to collect samples than doing it from a for-loop in R.  NIMBLE's full MCMC
## system collects samples internally, but we won't illustrate the nimbleFunction code for that now.

## An updated version of the function is this:
simpleMHbetter <- nimbleFunction(
    setup = function(model, node, proposalSD) {
        calcNodes <- model$getDependencies(node)
        savedValues <- modelValues(model, 1)
        nimCopy(from = model, to = savedValues, row = 1, logProb = TRUE)
    },
    run = function() {
        currentValue <- model[[node]]
        proposalValue <- rnorm(1, currentValue, proposalSD)
        model[[node]] <<- proposalValue
        logAcceptanceProbability <- calculateDiff(model, calcNodes)
        accept <- decide(logAcceptanceProbability)
        if(!accept) {
            nimCopy(from = savedValues, nodes = calcNodes, to = model, row = 1, logProb = TRUE)
        }
        return()
        returnType(void())
    })

simpleMHx_better <- simpleMH(simpleModel, 'x', proposalSD = 0.8)
xPosterior3 <- numeric(100)
uncompiledTime3 <- system.time(
    for(i in 1:100) {
        simpleMHx_better$run()
        xPosterior3[i] <- simpleModel$x
    }
)
plot(xPosterior3, type = 'b')
uncompiledTime3
## The time is about the same, but in more complicated models copying will be faster than calculating,
## especially after compilation

## Now let's compile and run it.  We'll look at compiling in two steps
cSimpleModel <- compileNimble(simpleModel)
cSimpleMHx_better <- compileNimble(simpleMHx_better, project = simpleModel)

xPosterior4 <- numeric(100)
compiledTime4 <- system.time(
    for(i in 1:100) {
        cSimpleMHx_better$run()
        xPosterior4[i] <- cSimpleModel$x
    }
)
plot(xPosterior4, type = 'b')
compiledTime4
uncompiledTime3[3] / compiledTime4[3]
## That was "only" about 60 times faster on my machine, but compilation becomes increasingly better for more complex models.

## Using NIMBLE's MCMC library
## Now let's see how we would set up an MCMC for the pump model
## First we can make a configuration, which just means a set of choices
## for what kind of samplers to use for which nodes.  NIMBLE
## will generate a default set of choices, and we can them modify them.
pumpMCMCconf <- configureMCMC(pump)
pumpMCMCconf$getSamplers()
## We see that conjugate (i.e. Gibbs) samplers have been chosen for each theta dimension (as well as beta)
## let's suppose for some reason we want to use random walk Metropolis-Hastings on theta[1]...theta[5].
## We could modify the choices as follows:
pumpMCMCconf$removeSamplers('theta[1:5]')
## It has shown us what remains
## Now add the new choices
for(targetNode in pump$expandNodeNames('theta[1:5]'))
    pumpMCMCconf$addSampler(target = targetNode, type = 'RW')
## Check the complete list
pumpMCMCconf$getSamplers()
## Also by default only top level nodes are recorded (monitored) for output.
## For this model let's add the thetas for monitoring
pumpMCMCconf$addMonitors('theta')

## Once we are happy with the sampler choices, we can build the MCMC
pumpMCMC <- buildMCMC(pumpMCMCconf)
## and compile and run it
cPumpMCMC <- compileNimble(pumpMCMC, project = pump)
cPumpMCMC$run(1000)
samples <- as.matrix(cPumpMCMC$mvSamples)
colnames(samples)
dim(samples)
plot(samples[,'alpha'], samples[,'beta'])

## Improving MCMC
## There are several strategies to improving MCMC using NIMBLE
## We'll talk more about how to measure the performance of different methods.
## Right now we'll just focus on setting up different methods.

## First, we can change the samplers.  Often a good method is to sample some nodes in a block.
## That means that  multivariate normal proposal will be used.
pumpMCMCconf2 <- configureMCMC(pump)
pumpMCMCconf2$removeSamplers(c('alpha', 'beta'))
pumpMCMCconf2$addSampler(target = c('alpha','beta'), type = 'RW_block')
pumpMCMC2 <- buildMCMC(pumpMCMCconf2)
cPumpMCMC2 <- compileNimble(pumpMCMC2, project = pump, resetFunctions = TRUE) ## we need resetFunctions = TRUE when building 2nd MCMC for the same model
cPumpMCMC2$run(1000)
samples2 <- as.matrix(cPumpMCMC2$mvSamples)
plot(samples2[,'alpha'], samples2[,'beta'])

## NIMBLE provides an autoBlock function, and an argument to configureMCMC that will use it
## that will automatically search for a good blocking scheme for a particular model.
## This can boost MCMC efficiency a lot in some cases, although at the moment it can take appreciable time to
## determine the blocks.

## The other main kind of sampler built into NIMBLE at this point is called a slice sampler, so give those as well.

## To illustrate more strategies, I'll set up a simplified version of the basic idea of an occupancy model
occCode <- nimbleCode({
    psi ~ dunif(0,1)
    for(i in 1:5) ## 5 sites
        z[i] ~ dbern(psi)
    detectionProb <- 0.2
    for(i in 1:5)
        for(j in 1:4) ## 4 visits per site
            y[i,j] ~ dbern(z[i] * detectionProb)
})

y <- matrix(rbinom(20, prob = .3, size = 1), nrow = 5) 
y[5,] <- rep(0, 4) ## ensure we have an all-zero capture history to keep z interesting## z should be provided as data when there is at least one observation -- normal practice
z <- as.numeric(rowSums(y) >= 1)
z[z == 0] <- NA ## NA means it should be sampled
y
z
occModel <- nimbleModel(code = occCode, data = list(y = y, z = z) )

occMCMCconf <- configureMCMC(occModel)
occMCMCconf$getSamplers()

## We see that a slice sampler has been assigned to z[5] (the only z dimension that needs sampling).
## We won't introduce slice sampling but just not that it is not a particularly great choice in this case.
## The reason is that z can only take values of 0 or 1, so it would be more efficient to always consider both values and sample
## from their full conditional distribution.
## Here is code for our own sampler that will work within NIMBLE's MCMC system:
custom_z_sampler <- nimbleFunction(
    contains = sampler_BASE,      ## required (class inheritance)
    setup = function(model, mvSaved, target, control) { ## required arguments
        calcNodes <- model$getDependencies(target)
        logProbs <- c(0.0,0.0)   ## to be used for logProbs for values of 0 and 1 respectively
    },
    run = function() {
        currentValue <- model[[target]]  ## get the current value
        currentLogProb <- getLogProb(model, calcNodes) ## and the related sum of logProb's
        for(i in 1:2) {   ## check both possible values
            if(i-1 == currentValue)  ## for the current value, record the current sum of logProb
                logProbs[i] <<- currentLogProb
            else {
                model[[target]] <<- i-1  ## for the other value, put it in the model
                logProbs[i] <<- calculate(model, calcNodes)  ## and calculate the sum of logProb's
            }
        }
        u <- runif(1, 0, 1)  ## pick a random number between 0 and 1
        if(u < exp(logProbs[1])/sum(exp(logProbs[1:2])))  ## choose 0 with this probability, 1 otherwise
            newValue <- 0
        else
            newValue <- 1
        
        if(newValue != currentValue) ## update the saved model states
            copy(from = model, to = mvSaved, row = 1, nodes = calcNodes, logProb = TRUE)
        else
            copy(from = mvSaved, to = model, row = 1, nodes = calcNodes, logProb = TRUE)
    },
    methods = list(  ## required for the MCMC system, but it doesn't have to do anything
        reset = function () {}
    )
    )

## Remove the old z[5] sampler and replace it with our new one in the configuration
occMCMCconf$removeSamplers('z[5]')
occMCMCconf$addSampler(target = 'z[5]', type = custom_z_sampler)
occMCMCconf$getSamplers()
occMCMCconf$getMonitors()
occMCMCconf$addMonitors('z')

## Compile and run
cOccModel <- compileNimble(occModel)
occMCMC <- buildMCMC(occMCMCconf)
cOccMCMC <- compileNimble(occMCMC, project = occModel)

cOccMCMC$run(1000)
occSamples <- as.matrix(cOccMCMC$mvSamples)
dim(occSamples)
plot(occSamples[,'psi'])
plot(jitter(occSamples[,'z[5]']))

## Re-writing the model with user-defined functions and distributions
## What is even better is to rewrite the model in ways that require fewer latent states
## and/or group some calculations together.
##
## In this case, we can easily write the probability of the detection history at a site in a way
## that directly sums of the possibilities of z = 0 or z = 1.  It is:
## Prob(all y[i,1:4] == 0) = Prob(all y[i,1:4] == 0 | z = 1) Prob(z == 1) + 1 * Prob(z == 0)
## (where I wrote "1" for Prob(y[i,1:4] == 0 | z = 0)
## and, for some non-zero detections:
## Prob(y[i,1:4]) = Prob(y[i,1:4] | z = 1) Prob(z == 1) + 0 * Prob(z == 0)
## (where I wrote "0" for Prob(y[i,1:4] | z==0) since if the site is unoccupied we can't get some non-zero detections
## Let's write that in a nimbleFunction

## Note that it is tempting to sum the y[i, 1:4] and use the sum in a binomial probability calculation.
## I am not going to do that because the model is often generalized to include occassion-specific detection probability
## and I want to preserve that an as extension to what I'm doing

dOccupancy <- nimbleFunction( ## x is the detection history
    run = function(x = double(1), occupancyProb = double(), detectionProb = double(), numVisits = double(), log = integer(0, default = 0)) {
        returnType(double())
        logProbDetectionHistoryIfOccupied <- 0
        allZero <- 1
        for(i in 1:numVisits) {
            if(x[i] == 0)
                logProbDetectionHistoryIfOccupied <- logProbDetectionHistoryIfOccupied + log(1-detectionProb)
            else {
                logProbDetectionHistoryIfOccupied <- logProbDetectionHistoryIfOccupied + log(detectionProb)
                allZero <- 0
            }
        }
        ansProb <- exp(logProbDetectionHistoryIfOccupied) * occupancyProb + allZero * (1-occupancyProb)
        if(log)
            return(log(ansProb))
        else
            return(ansProb)
    })

rOccupancy <- nimbleFunction( ## Our algorithm may not use simulation, but we are required to provide an "r" function.
    ## n is required but currently ignored
    run = function(n = integer(), occupancyProb = double(), detectionProb = double(), numVisits = double()) {
        returnType(double(1))
        declare(retVec, double(1))
        setSize(retVec, numVisits)
        z <- rbinom(1, prob = occupancyProb, size = 1)
        if(z == 0) { ## unoccupied, so detection history must be all 0
            for(i in 1:numVisits)
                retVec[i] <- 0 
        } else { ## occupied
            for(i in 1:numVisits)
                retVec[i] <- rbinom(1, prob = detectionProb, size = 1)                
        }
        return(retVec)
    })

## We provide these for use in models like this (a bit laborious at the moment):
registerDistributions(list(dOccupancy = list(
    BUGSdist = "dOccupancy(occupancyProb, detectionProb, numVisits)",
    Rdist = "dOccupancy(occupancyProb, detectionProb, numVisits)",
    types = c('value = double(1)', 'occupancyProb = double()', 'detectionProb = double()', 'numVisits = integer()'))
  ))

## Now we can write the model like this:
occCode2 <- nimbleCode({
    psi ~ dunif(0,1)
    detectionProb <- 0.2
    for(i in 1:5)
        y[i, 1:4] ~ dOccupancy(psi, detectionProb, 4)
})

occModel2 <- nimbleModel(occCode2, data = list(y = y))
## We can skip over the configureMCMC step if we don't want to modify it
occMCMC2 <- buildMCMC(occModel2)
cOccModel2 <- compileNimble(occModel2)
cOccMCMC2 <- compileNimble(occMCMC2, project = occModel2)

cOccMCMC2$run(1000)
occSamples2 <- as.matrix(cOccMCMC2$mvSamples)
colnames(occSamples2)
plot(occSamples2[,'psi'])

##  We'd have to look carefully at whether this has actually improved MCMC efficiency in this particular case.
## For our purposes the point is that with this kind of flexibility one can sometimes make large improvements. 

## Some of other ways NIMBLE extends the BUGS language include:
## You can use alternative parameterizations of distributions, not just the single option provided by BUGS
## You can use if-then-else statements that will be evaluated when the model is created, so that the
## same model code can define alternative models.
## You can use named arguments for distributions or functions, much like in R.

