
#Calculating Maximum likelihood estimator then draw the graph of cdf, pdf, S(t)

set.seed(1234) 
x <- rnorm(1000, mean = 2, sd =9)

normal.log.lik<-function(theta){
mu<-theta[1]
sigma<-theta[2]
n=length(x)

log.lik <-  sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))    # Calculate the log-likelihood
  
  return(-log.lik)  #     Return the negative log-likelihood (to minimize)
}

MLE<-optim(c(mu=mean(x),sigma=sd(x)),normal.log.lik)
MLE$par

mu<- 1.757
sigma<- 8.971


F <- pnorm(x,mu,sigma,lower.tail=TRUE, log.p=FALSE)
f <- dnorm(x,mu,sigma,log=FALSE)
S <- 1-F
plot(x,F)
plot(x,S)
plot(x,f)

################ B10 life of weibull dist @@@@@@@@@@@@@@@@@@

beta <- 1.02
alfa <- 2364

B.10 <- alfa*((-log(1-0.1))^(1/beta))



################ MTTF of weibull dist @@@@@@@@@@@@@@@@@@

MTTF <- alfa*gamma(1+1/beta)



