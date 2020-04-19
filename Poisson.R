#The Poisson Distribution

n<-100
x<-seq(0,n,1)
lambda1<-.05
lambda2<-.5
lambda3<-1
lambda4<-3
lambda5<-5
lambda6<-50
#Find random values from a sample of 'size'

plot(dpois(x,lambda1),ylab="density",type="l",lwd=3,col=6,xlim=c(0,20),ylim=c(0,1),main="Poisson Distributions")
lines(dpois(x,lambda2),type="l",lwd=3,col=5)
lines(dpois(x, lambda3),type="l",lwd=3,col=4)
lines(dpois(x, lambda4),type="l",lwd=3,col=3)
lines(dpois(x, lambda5),type="l",lwd=3,col=2)
lines(dpois(x, lambda6),type="l",lwd=3,col=1)
legend(13,.60,c("Poisson(n,lambda1)","Poisson(n,lambda2)","Poisson(n,lambda3)","Poisson(n,lambda4)","Poisson(n,lambda5)","Poisson(n,lambda6)"),
       lty=c(1,1,1,1,1,1),lwd=c(3,3,3,3,3,3),col=c(6,5,4,3,2,1))

#Generate 100 random poisson variables
n<-100
x<-rpois(n,lambda5)
#Find the average x_bar
x_bar<-sum(x)/n
estlambda<-x_bar


#Plot both known distributions and their estimated distributions
p <-seq(0,n,by=1)
plot(p, dpois(p,lambda5),ylab="density",type="l",lwd=3,col=2,ylim=c(0,1),xlim=c(0,20),main="Poisson Distributions")
lines(p, dpois(p,estlambda),type="l",lwd=3,col=1)
legend(10,.60,c("Poisson(n,lambda1)","Poisson(n,estlambda)"),lty=c(1,1),lwd=c(3,3),col=c(2,1))

