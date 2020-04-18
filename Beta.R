#The Beta Distribution
p = seq(0,1,length=100)
plot(p, dbeta(p,2,2),ylab="density",type="l",lwd=3,col=6,ylim=c(0,3),main="Beta Distributions")
lines(p, dbeta(p,2,6),type="l",lwd=3,col=5)
lines(p, dbeta(p, 0.5, 0.5),type="l",lwd=3,col=4)
lines(p, dbeta(p, 0.2,0.5),type="l",lwd=3,col=3)
lines(p, dbeta(p, 0.2,2),type="l",lwd=3,col=2)
lines(p, dbeta(p, 1,1),type="l",lwd=3,col=1)
legend(.7,2.75,c("B(2,2)","B(2,6)","B(0.5,0.5)","B(0.2,0.5)","B(0.2,2)","B(1,1)"),
       lty=c(1,1,1,1,1,1),lwd=c(3,3,3,3,3,3),col=c(6,5,4,3,2,1))



# Generate 100 random beta variables with shape parameter 3 and shape parameter 3
n<-100
shape1<-2.25
shape2<-.3
x<-rbeta(n,shape1,shape2)
#Find the average x_bar
x_bar<-sum(x)/n
#Find the squared difference between randomly observed beta variables and x_bar
sqdiff<-(x-x_bar)^2
#sum it up
x1<-sum(sqdiff)
#divide by n-1
samplevariance<-x1/(n-1)
#Estimate shape 1 
est1<-((x_bar*(1-x_bar))/(samplevariance)-1)
shape1_est<-x_bar*est1


#Estimate shape 2
est2<-((x_bar*(1-x_bar))/(samplevariance)-1)
shape2_est<-(1-x_bar)*est2

#Again with different initial conditions
# Generate 100 random beta variables with shape parameter 3 and shape parameter 3
n<-100
shape1<-.05
shape2<-7
x<-rbeta(n,shape1,shape2)
#Find the average x_bar
x_bar<-sum(x)/n
#Find the squared difference between randomly observed beta variables and x_bar
sqdiff<-(x-x_bar)^2
#sum it up
x1<-sum(sqdiff)
#divide by n-1
samplevariance<-x1/(n-1)
#Estimate shape 1 
est1<-((x_bar*(1-x_bar))/(samplevariance)-1)
shape1_est<-x_bar*est1


#Estimate shape 2
est2<-((x_bar*(1-x_bar))/(samplevariance)-1)
shape2_est<-(1-x_bar)*est2


#Plot both known distributions and their estimated distributions
p = seq(0,1,length=100)
plot(p, dbeta(p,shape1,shape2),ylab="density",type="l",lwd=3,col=2,main="Beta vs Estimated Beta")
lines(p, dbeta(p, shape1_est, shape2_est),type="l",lwd=3,col=1)
legend(.7,1,c("B(shape1,shape2)","B(shape1_est,shape2_est)",
       lty=c(1,1),lwd=c(3,3),col=c(2,1)))

getwd()
       
       