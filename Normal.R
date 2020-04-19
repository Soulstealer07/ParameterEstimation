#The Normal Distribution
n<-100

p<-seq(from=-10,to =15, by=.10)
head(p)
mean1<-0
mean2<--1
mean3<-1
mean4<--7
mean5<-7
mean6<-9
stdev1<-1
astdev2<-.5
stdev3<-2
stdev4<-.5
stdev5<-.25
stdev6<-.9
plot(p, dnorm(p,mean1,stdev1),ylab="density",type="l",lwd=3,col=6,xlim=c(-10,15),ylim=c(0,3),main="Normal Distributions")
lines(p, dnorm(p,mean2,stdev2),type="l",lwd=3,col=5)
lines(p, dnorm(p, mean3, stdev3),type="l",lwd=3,col=4)
lines(p, dnorm(p, mean4,stdev4),type="l",lwd=3,col=3)
lines(p, dnorm(p, mean5,stdev5),type="l",lwd=3,col=2)
lines(p, dnorm(p, mean6,stdev6),type="l",lwd=3,col=1)
legend(-5,2.5,c("Noraml(mean1,stdev1)","Normal(mean2,stdev2)","Normal(mean3,stdev3)","Normal(mean4,stdev4)","Normal(mean5,stdev5)","Normal(mean6,stdev6)"),
       lty=c(1,1,1,1,1,1),lwd=c(3,3,3,3,3,3),col=c(6,5,4,3,2,1))


# Generate 100 random normal variables with scale parameter Mu and shape parameer sigma
#Estimate Mu
n<-100
Mu<-mean1
sigma<-stdev1
x<-rnorm(n,Mu,sigma)
#Find the average x_bar
x_bar<-sum(x)/n
Mu_est<-x_bar

#Estimate sigma
sigma_est<-(x-Mu)^2
sigma_est<-sum(sigma_est)
sigma_est<-sigma_est/n
sigma_est


#Plot both known distributions and their estimated distributions
p<-seq(from=-10,to =15, by=.10)
plot(p, dnorm(p,mean1,stdev1),ylab="density",type="l",xlim=c(-10,15),ylim=c(0,3),lwd=3,col=2,main="Normal vs Estimated Normal")
lines(p, dnorm(p, Mu_est, sigma_est),type="l",lwd=3,col=1)
legend(3,2.5,c("Normal(mean1,stdev1)","Normal(Mean_est,sigma_est)"),
              lty=c(1,1),lwd=c(3,3),col=c(2,1))



