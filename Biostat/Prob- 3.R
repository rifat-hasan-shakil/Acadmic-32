
#Calculating Maximum likelihood estimator for censored then draw the graph of cdf, pdf, S(t)

## For normal distribution

#fdata <- read.csv("test1.csv", as.is=TRUE)
fdata <- read.csv("C:\\Users\\User\\Desktop\\test1.csv", as.is=TRUE)	# Read data from file

t <- fdata$Days    						# failure/censored time
d <- fdata$Status   
n <- length(t)  


mu <- mean(t)
sigma <- sd(t)

normal.log.lik<-function(theta){
mu<-theta[1]
sigma<-theta[2]

# Calculate the log-likelihood

log.lik.censored <- sum( d*log(dnorm(t, mean = mu, sd = sigma, log = FALSE))
    + (1-d)*log(1-pnorm(t, mean = mu, sd = sigma, lower.tail = TRUE, log.p =  FALSE)))

  return(-log.lik.censored)  #     Return the negative log-likelihood (to minimize)
}

MLE.norm<-optim(c(mu, sigma),normal.log.lik )

MLE.norm$par



#### __________________Using survreg formula______________

library(survival)

fit1 <- survreg(Surv(t, d) ~ 1,  dist='gaussian')   
fit1

mean <-fit1$coefficient
mean
sigma <- fit1$scale
sigma 


#==================================================================================
# Solving same problem applying Weibull dist


shape <- 1.3
scale <- 18.0

weibull.log.lik<-function(theta){
shape <- theta[1]
scale <- theta[2]

# Calculate the log-likelihood


log.lik.censored <- sum( d*log(dweibull(t, shape=theta[1], scale =theta[2], log = FALSE))
    + (1-d)*log(1-pweibull(t, shape=theta[1], scale =theta[2], lower.tail = TRUE, log.p =  FALSE)))

  return(-log.lik.censored)  #     Return the negative log-likelihood (to minimize)
}

MLE.weib<-optim(c(shape,scale),weibull.log.lik )

MLE.weib$par

#### __________________Using survreg formula______________

library(survival)

fit2 <- survreg(Surv(t, d) ~ 1,  dist='weibull')   
fit2

beta.hat  <- 1/fit2$scale
beta.hat 
eta.hat <- exp(fit2$coefficient)
eta.hat


#==================================================================================
# Solving same problem applying Exponential dist

lambda<- 1/mean(t)

exponential.log.lik<-function(theta){
lambda <- theta

# Calculate the log-likelihood


log.lik.censored <- sum( d*log(dexp(t, lambda , log = FALSE))
    + (1-d)*log(1-pexp(t, lambda , lower.tail = TRUE, log.p =  FALSE)))

  return(-log.lik.censored)  #     Return the negative log-likelihood (to minimize)
}

MLE.exp<-optim(c(lambda),exponential.log.lik )

MLE.exp$par

#### __________________Using survreg formula______________

library(survival)

fit3 <- survreg(Surv(t, d) ~ 1,  dist='exponential')   
fit3

lambda.hat  <- 1/exp(fit3$coefficient)
lambda.hat 






