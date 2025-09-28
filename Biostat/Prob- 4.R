
library(survival)

# Choose the CSV file using file.choose()
file_path <- file.choose()             # Choose test.2.csv file

# Read the CSV file into a data frame
data <- read.csv(file_path)

# Print the first few rows of the data frame
#head(data)
time <- data$time
ind <-  data$ind
freq <- data$freq
 

KM.fit <- survfit(Surv(time,ind)~1,weight=freq,type="kaplan-meier")

summary(KM.fit)
t <- summary(KM.fit)$time
KM.rel <- summary(KM.fit)$surv
KM.F <- 1-KM.rel


 beta <- 1.56
 alfa <- 44.94

W.F <- pweibull(t,beta,alfa,lower.tail=TRUE,log.p=FALSE)
W.rel <- 1-W.F
transform(table(W.rel))

lambda <- .02
E.F <- pexp(t,lambda,lower.tail=TRUE,log.p=FALSE)
E.rel <- 1-E.F



plot(t,KM.F, main="Comparison of CDFs", xlab=" Time", ylab="CDF", col=1, lty= 1, type="s", lwd=1)
lines(t,W.F, col=2, lty=2,type="l", lwd=2)
lines(t,E.F, col=3, lty=3,type="l", lwd=2)
legend("bottomright", c("KM cdf", "Weibull cdf","Exponential cdf"), col=c(1,2,3), text.col=c(1,2,3), lty=c(1,2,2),lwd=2)
abline(h=0.5,col=2)  # finding the median time 
abline(v=591.215,col=2)


 
