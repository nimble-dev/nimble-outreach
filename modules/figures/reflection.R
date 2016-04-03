x <- seq(-3, 7, len = 100)
fx <- dnorm(x, 2, sd = 1.5)

jpeg('reflection.jpg', width = 400, height=400)

plot(x, fx, type = 'l', xlab = expression(theta), ylab = 'density')
segments(2,-0.1,2,0.02, col = 'black')
segments(-1,-0.1,-1,0.02, col = 'red')
segments(1,-0.1,1,0.035, col = 'green')
abline(v = 0, col = 'grey')
text(3, .2, "proposal density")
text(-0.2, .15, "truncation at b=0", srt = -270)
text(2, .03, "current")
text(-1, .03, "initial proposal", col = 'red')
text(1, .045, "reflected proposal", col = 'green')

dev.off()         
