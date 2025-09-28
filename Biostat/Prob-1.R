###Calculate and draw the graph of cdf, pdf, S(t) and h(t)

t <- seq(0,2,0.2)
t

## functions of Normal distribution
mu=3
sigma= 9

f.norm <- dnorm(t,mu,sigma,log=FALSE)
F.norm <- pnorm(t,mu,sigma,lower.tail=TRUE, log.p=FALSE)
S.norm <- 1-F.norm
h.norm <- f.norm /S.norm 

plot(t,f.norm,main="pdf of Normal dist", xlab="time(t)",ylab="pdf")
plot(t,F.norm,main="cdf of Normal dist", xlab="time(t)",ylab="cdf")

## functions of Exponential distribution
theta <- .12

f.exp <- dexp(t,theta,log=FALSE)
F.exp <- pexp(t,theta,lower.tail=TRUE, log.p=FALSE)
S.exp <- 1-F.exp
h.exp <- f.exp /S.exp 



# functions of Weibull distribution
beta <- 1.5
alfa <- 18

f.weibull <- dweibull(t,beta,alfa,log=FALSE)
F.weibull <- pweibull(t,beta,alfa,lower.tail=TRUE, log.p=FALSE)
S.weibull <- 1-F.weibull
h.weibull <- f.weibull/S.weibull

plot(t,f.weibull,main="pdf of Weibull distributions", xlab="time(t)",ylab="pdf",col=1,lty=1)






