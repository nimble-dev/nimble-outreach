## ----eval = FALSE--------------------------------------------------------
## {
## 
## # -------------------------------------------------
## # Parameters:
## # s: survival probability
## # psiV: transitions from vegetative
## # psiF: transitions from flowering
## # psiD: transitions from dormant
## # -------------------------------------------------
## # States (S):
## # 1 vegetative
## # 2 flowering
## # 3 dormant
## # 4 dead
## # Observations (O):
## # 1 seen vegetative
## # 2 seen flowering
## # 3 not seen
## # -------------------------------------------------
## 
## # Priors and constraints
##    # Survival: uniform
##    for (t in 1:(n.occasions-1)){
##       s[t] ~ dunif(0, 1)
##       }
##    # Transitions: gamma priors
##    for (i in 1:3){
##       a[i] ~ dgamma(1, 1)
##       psiD[i] <- a[i]/sum(a[])
##       b[i] ~ dgamma(1, 1)
##       psiV[i] <- b[i]/sum(b[])
##       c[i] ~ dgamma(1, 1)
##       psiF[i] <- c[i]/sum(c[])
##       }
## 
## # Define state-transition and observation matrices 	
## for (i in 1:nind){
##    # Define probabilities of state S(t+1) given S(t)
##    for (t in 1:(n.occasions-1)){
##       ps[1,i,t,1] <- s[t] * psiV[1]
##       ps[1,i,t,2] <- s[t] * psiV[2]
##       ps[1,i,t,3] <- s[t] * psiV[3]
##       ps[1,i,t,4] <- 1-s[t]
##       ps[2,i,t,1] <- s[t] * psiF[1]
##       ps[2,i,t,2] <- s[t] * psiF[2]
##       ps[2,i,t,3] <- s[t] * psiF[3]
##       ps[2,i,t,4] <- 1-s[t]
##       ps[3,i,t,1] <- s[t] * psiD[1]
##       ps[3,i,t,2] <- s[t] * psiD[2]
##       ps[3,i,t,3] <- s[t] * psiD[3]
##       ps[3,i,t,4] <- 1-s[t]
##       ps[4,i,t,1] <- 0
##       ps[4,i,t,2] <- 0
##       ps[4,i,t,3] <- 0
##       ps[4,i,t,4] <- 1
## 
##       # Define probabilities of O(t) given S(t)
##       po[1,i,t,1] <- 1
##       po[1,i,t,2] <- 0
##       po[1,i,t,3] <- 0
##       po[2,i,t,1] <- 0
##       po[2,i,t,2] <- 1
##       po[2,i,t,3] <- 0
##       po[3,i,t,1] <- 0
##       po[3,i,t,2] <- 0
##       po[3,i,t,3] <- 1
##       po[4,i,t,1] <- 0
##       po[4,i,t,2] <- 0
##       po[4,i,t,3] <- 1
##       } #t
##    } #i
## 
## # Likelihood
## for (i in 1:nind){
##    # Define latent state at first capture
##    z[i,f[i]] <- y[i,f[i]]
##    for (t in (f[i]+1):n.occasions){
##       # State process: draw S(t) given S(t-1)
##       z[i,t] ~ dcat(ps[z[i,t-1], i, t-1,])
##       # Observation process: draw O(t) given S(t)
##       y[i,t] ~ dcat(po[z[i,t], i, t-1,])
##       } #t
##    } #i
## }

## ----eval=FALSE----------------------------------------------------------
## ## Filter MCMC for the orchid model
## 
## ## load nimble library
## library(nimble)
## 
## ## define custom distribution
## dDHMMorchid <- nimbleFunction(
##     run = function(x = double(1), length = double(),
##     	  	   prior = double(1), Z = double(2),
## 		   T = double(3), log.p = double()) {
##         pi <- prior
##         logL <- 0
##         for(t in 1:length) {
##             Zpi <- Z[x[t], ] * pi
##             sumZpi <- sum(Zpi)
##             logL <- logL + log(sumZpi)
##             if(t != length)
## 	       pi <- (T[,,t] %*% Zpi / sumZpi)[ ,1]
##         }
##         returnType(double())
##         return(logL)
##     }
## )
## 
## rDHMMorchid <- nimbleFunction(
##     run = function(n = integer(), length = double(),
##                    prior = double(1), Z = double(2),
## 		   T = double(3)) {
##         declare(x, double(1, length))
##         returnType(double(1))
##         return(x)
##     }
## )
## 
## registerDistributions(list(
##     dDHMMorchid = list(
##         BUGSdist = 'dDHMMorchid(length, prior, Z, T)',
##         types = c('value = double(1)', 'length = double()', 'prior = double(1)', 'Z = double(2)', 'T = double(3)'),
##         discrete = TRUE
##     )
## ))
## 
## ## define hierarchical model
## code <- nimbleCode({
##     for (t in 1:(k-1)) {
##         s[t] ~ dunif(0, 1)
##     }
##     for (i in 1:3) {
##         a[i] ~ dgamma(1, 1)
##         psiD[i] <- a[i]/sum(a[1:3])
##         b[i] ~ dgamma(1, 1)
##         psiV[i] <- b[i]/sum(b[1:3])
##         c[i] ~ dgamma(1, 1)
##         psiF[i] <- c[i]/sum(c[1:3])
##     }
##     for (t in 1:(k-1)) {
##         T[1,1,t] <- s[t] * psiV[1]
##         T[2,1,t] <- s[t] * psiV[2]
##         T[3,1,t] <- s[t] * psiV[3]
##         T[4,1,t] <- 1-s[t]
##         T[1,2,t] <- s[t] * psiF[1]
##         T[2,2,t] <- s[t] * psiF[2]
##         T[3,2,t] <- s[t] * psiF[3]
##         T[4,2,t] <- 1-s[t]
##         T[1,3,t] <- s[t] * psiD[1]
##         T[2,3,t] <- s[t] * psiD[2]
##         T[3,3,t] <- s[t] * psiD[3]
##         T[4,3,t] <- 1-s[t]
##         T[1,4,t] <- 0
##         T[2,4,t] <- 0
##         T[3,4,t] <- 0
##         T[4,4,t] <- 1
##     }
##     T[1,1,k] <- 1
##     T[2,1,k] <- 0
##     T[3,1,k] <- 0
##     T[4,1,k] <- 0
##     T[1,2,k] <- 0
##     T[2,2,k] <- 1
##     T[3,2,k] <- 0
##     T[4,2,k] <- 0
##     T[1,3,k] <- 0
##     T[2,3,k] <- 0
##     T[3,3,k] <- 1
##     T[4,3,k] <- 0
##     T[1,4,k] <- 0
##     T[2,4,k] <- 0
##     T[3,4,k] <- 0
##     T[4,4,k] <- 1
##     for (i in 1:nind) {
##         y[i, f[i]:k] ~ dDHMMorchid(length = k-f[i]+1,
## 	                           prior = prior[1:4],
## 				   Z = Z[1:3,1:4],
## 				   T = T[1:4,1:4,f[i]:k])
##     }
## })
## 

