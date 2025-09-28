
#### Problem 1-STRATIFIED SAMPLING### yes
##SAMPLE(strata) GIVEN###

###Using function
st1=c(165,161,153,150,151,153)
st2=c(157,161,168,162,165,171,169,164)
st3=c(168,165,175,175,165,163,165,165)
st4=c(164,171,177,163,170,165,160,175)
st5=c(175,173,161,158,175,164,158,161,158,171,175,170,187,168,170,185)
st6=c(190,178,194,183,165,170,176,173,168,183,173,183,174,177)
Ni=c(53,65,75,79,145,127)

str=function(st1,st2,st3,st4,st5,st6,Ni){
     n1=length(st1);n2=length(st2);n3=length(st3);n4=length(st4);n5=length(st5);n6=length(st6)
     ni=c(n1,n2,n3,n4,n5,n6)
     N=sum(Ni)
     wi=Ni/N
     fi=ni/Ni
     strata.mean=c(mean(st1),mean(st2),mean(st3),mean(st4),mean(st5),mean(st6))
     mean=sum(wi*strata.mean)
     
     si_sq=c(var(st1),var(st2),var(st3),var(st4),var(st5),var(st6))
     var=sum(wi^2*(1-fi)*si_sq/ni)
     ztab=qnorm(0.05/2,lower.tail=FALSE)
     LCL=mean-ztab*sqrt(var)
     UCL=mean+ztab*sqrt(var)
   return(cbind(Mean.st=mean,Variance.st=var,LCL,UCL))
 }
str(st1,st2,st3,st4,st5,st6,Ni)


#--------------------------------------------------------------------------------------------------------------------
### Problem 2-STRATIFIED SAMPLING####
###POP(strata) GIVEN SAMPLE DRAW FROM POP(strata)##### 
##Using function
st1=c(165,161,153,150,151,153)
st2=c(157,161,168,162,165,171,169,164)
st3=c(168,165,175,175,165,163,165,165)
st4=c(164,171,177,163,170,165,160,175)
st5=c(175,173,161,158,175,164,158,161,158,171,175,170,187,168,170,185)
st6=c(190,178,194,183,165,170,176,173,168,183,173,183,174,177)
n1=3;n2=3;n3=4;n4=3;n5=6;n6=5
ni=c(n1,n2,n3,n4,n5,n6)

str=function(st1,st2,st3,st4,st5,st6,ni){
      N1=length(st1);N2=length(st2);N3=length(st3);N4=length(st4);N5=length(st5);N6=length(st6)
      Ni=c(N1,N2,N3,N4,N5,N6)
      N=sum(Ni)
      wi=Ni/N
      fi=ni/Ni
      set.seed(1234)
      sample.st1=sample(st1,n1)
      sample.st2=sample(st2,n2)
      sample.st3=sample(st3,n3)
      sample.st4=sample(st4,n4)
      sample.st5=sample(st5,n5)
      sample.st6=sample(st6,n6)
      strata.mean=c(mean(sample.st1),mean(sample.st2),mean(sample.st3),mean(sample.st4),mean(sample.st5),mean(sample.st6))
      mean=sum(wi*strata.mean)
      
      Si_sq=c(var(st1),var(st2),var(st3),var(st4),var(st5),var(st6))
      var.strata=sum(wi^2*(1-fi)*Si_sq/ni)

      si_sq=c(var(sample.st1),var(sample.st2),var(sample.st3),var(sample.st4),var(sample.st5),var(sample.st6))  #For sample
      var.est=sum(wi^2*(1-fi)*si_sq/ni)
       
      ztab=qnorm(0.05/2,lower.tail=FALSE)
      LCL=mean-ztab*sqrt(var.est)
      UCL=mean+ztab*sqrt(var.est)
   return(cbind(Mean.strata=mean,Variance.strata=var.strata,Variance.est=var.est,LCL,UCL))
 }
str(st1,st2,st3,st4,st5,st6,ni)  


#-------------------------------------------------------------------------------------------
###### Problem 3- CLUSTER SAMPLING##### 

#N=number of cluster in population
#M=number of elements in population
#m.vec=vector of cluster size in the population
#y=either a vector of totals per cluster 
#or a list of the obs per cluster(this is set by total)

##Using function

cluster.mu=function(N,m.vec,y,M=NA){
              n=length(m.vec)
              #if M is unknown m.bar is estimated with the mean of m.vec
             if(is.na(M)){
                m.bar=mean(m.vec) 
             }else{
                m.bar=M/N
             }
           mu.hat=sum(y)/sum(m.vec)
           s2.c=sum((y-(mu.hat*m.vec))^2)/(n-1)
           var.mu.hat=((N-n)/(N*m.bar^2))*s2.c
           B=2*sqrt(var.mu.hat)
        return(cbind(mu.hat,s2.c,var.mu.hat,B))
   }
m=c(8,12,4,5,6,6,7,5,8,3,2,6,5,10,9,3,6,5,5,4,6,8,7,3)
y=c(96,121,42,65,52,40,75,65,45,50,85,43,54,49,53,50,32,22,45,37,51,30,39,47)
cluster.mu(415,m.vec=m,y,M=NA)


#--------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------

###### Problem 4 Ratio Estimator ######### 
##using function
x=c(6.7,8.2,7.9,6.4,8.3,7.2,6.0,7.4,8.1,9.3,8.2,6.8,7.4,7.5,8.3,9.1,8.6,7.9,6.3,8.9)
y=c(7.1,8.4,8.2,6.9,8.4,7.9,6.5,7.6,8.9,9.9,9.1,7.3,7.8,8.3,8.9,9.6,8.7,8.8,7.0,9.4)
N=1000
Xbar=7.8
ratio=function(x,y,N){
         n=length(x)
         r=sum(y)/sum(x)
         var.r=(N-n)/(n*(n-1)*N*Xbar^2)*sum((y-r*x)^2)    #sampling variance of ratio estimator
         ztab=qnorm(0.05/2,lower.tail=FALSE)
         LCL=r-ztab*sqrt(var.r)
         UCL=r+ztab*sqrt(var.r) 
       return(cbind(ratio=r,variance.ratio=var.r,LCL,UCL))
  }
ratio(x,y,N)

#---------------------------------------------------------------------------------------------------

###### Problem 5 PPS sampling  ####### 
X=c(61,1079,30,1519,1036,48,2309,936,1018,811,1341,1715,561,133,1452,1134,567,1027,1169,1573,738,1799,2243)
Y=c(48,994,26,1313,779,40,1512,859,577,723,1121,928,483,662,1352,853,479,543,1093,1069,660,1753,1873)
N=length(Y)
n=5
## a ##
SRSWOR=function(X,Y,n){
	#y=sample(Y,n,replace=FALSE)
	y=c(483,723,40,1093,660)
	ybar=sum(y)/length(y)

	#An estimate of the total population is
	Y.hat=N*ybar
	Ybar=sum(Y)/N
	S_sq=sum((Y-Ybar)^2)/(N-1)
	var.Ybar=(N-n)/N*(S_sq/n)
	s_sq=sum((y-ybar)^2)/(n-1)
	var.ybar=(N-n)/N*(s_sq/n)
	se.ybar=sqrt(var.ybar)

	#95% CI for population mean
        ztab=qnorm(0.05/2,lower.tail=FALSE)
	LCL=ybar-ztab*se.ybar
	UCL=ybar+ztab*se.ybar
	return(rbind(ybar,Ybar,Y.hat,S_sq,var.Ybar,s_sq,var.ybar,se.ybar,LCL,UCL))
}
SRSWOR(X,Y,n)


cbind(X,Cum.sum=cumsum(X),Probability=round(X/sum(X),3))

#ran=sample(1:length(X),5,replace=FALSE);ran
ran=c(15,12,20,8,22)
yi=Y[ran]
xi=X[ran]
cbind(yi,xi)
Xt=sum(X)

ybar_pps=Xt/(N*n)*sum(yi/xi)
Yhat_pps=N*ybar_pps
var.ybar_pps=sum((Xt/N*(yi/xi)-ybar_pps)^2)/(n*(n-1))
Se.ybar_pps=sqrt(var.ybar_pps)
ztab=qnorm(0.05/2,lower.tail=FALSE)
LCL=ybar_pps-ztab*Se.ybar_pps
UCL=ybar_pps+ztab*Se.ybar_pps
cbind(ybar_pps,Yhat_pps,var.ybar_pps,LCL,UCL)





###### Problem 6 Double Sampling  ######

##a  and b
data=read.csv(file.choose())
data
double_sampling=function(data,i,d) {       # i=individuals sample,  d= days sample
                  set.seed(123)
                  individual=sample(data$ID,i,replace=FALSE)
                         day=sample(data$ID,d,replace=FALSE)

                  sampled_data=data[individual[day],]
               return(sampled_data)
      }
sampled_data=double_sampling(data=data,i=50,d=5);sampled_data

##c
ybar=mean(sampled_data$Calories);ybar
Ybar=mean(data$Calories);Ybar

#d
paste("The average daily calorie intake from double sampling data is=",ybar)
paste("The overall average daily calorie intake for entire population is=",Ybar)
paste("which moderate distance is=",abs(ybar-Ybar))








