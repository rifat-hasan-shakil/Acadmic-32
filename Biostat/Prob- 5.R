

data <- read.csv("test3.csv", as.is=TRUE)	# Read data from file

##_____________________ Before vaccination group

Btime <- data$Btime
Bind <-  data$Bind
Bfreq <- data$Bfreq
 

B.KM.fit <- survfit(Surv(Btime,Bind)~1,weight=Bfreq,type="kaplan-meier")
summary(B.KM.fit)

before.t <- summary(B.KM.fit)$time
B.KM.rel <-summary(B.KM.fit)$surv
B.KM.F <- 1-B.KM.rel


#_________________________After vaccination group

Atime <- data$Atime
Aind <-  data$Aind
Afreq <- data$Afreq
 

A.KM.fit <- survfit(Surv(Atime,Aind)~1,weight=Afreq,type="kaplan-meier")
summary(A.KM.fit)

after.t <- summary(A.KM.fit)$time
A.KM.rel <- summary(A.KM.fit)$surv
A.KM.F <- 1-A.KM.rel


plot (before.t, B.KM.F, main="Comparison of CDFs", xlab=" Time", ylab="CDF", col=1, lty= 1, type="s", lwd=1)
lines (after.t, A.KM.F, col=2, lty=1,type="s", lwd=2)
legend ("topleft", c("cdf of before vaccination group", "cdf of after vaccination group"), col=c(1,2), text.col=c(1,2), lty=c(1,1),lwd=2)







