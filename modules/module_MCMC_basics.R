## @knitr output-mcmc

Rsamples <- as.matrix(pumpMCMC$mvSamples)
samples <- as.matrix(CpumpMCMC$mvSamples)

identical(Rsamples, samples[1:5, ])

par(mfrow = c(1, 4), mai = c(.6, .5, .1, .2))
plot(samples[ , 'alpha'], type = 'l', xlab = 'iteration',
     ylab = expression(alpha), main = expression(alpha))
plot(samples[ , 'beta'], type = 'l', xlab = 'iteration',
     ylab = expression(beta), main = expression(beta))
plot(samples[ , 'alpha'], samples[ , 'beta'], xlab = expression(alpha),
     ylab = expression(beta), main = paste(expression(alpha), expression(beta), "dependence"))
plot(samples[ , 'theta[1]'], type = 'l', xlab = 'iteration',
     ylab = expression(theta[1]), main = expression(theta_1))
