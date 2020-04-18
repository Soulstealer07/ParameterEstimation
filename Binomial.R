#The Binomial Distribution
n<-100

p <- seq(0,n,by=1)
prob1<-.04
prob2<-.15
prob3<-.45
prob4<-.70
prob5<-.85
prob6<-.91
#Find random values from a sample of 'size'

plot(p, dbinom(p,n,prob1),ylab="density",type="l",lwd=3,col=6,ylim=c(0,.2),main="Binomial Distributions")
lines(p, dbinom(p,n,prob2),type="l",lwd=3,col=5)
lines(p, dbinom(p, n, prob3),type="l",lwd=3,col=4)
lines(p, dbinom(p, n, prob4),type="l",lwd=3,col=3)
lines(p, dbinom(p, n, prob5),type="l",lwd=3,col=2)
lines(p, dbinom(p, n, prob6),type="l",lwd=3,col=1)
legend(60,.20,c("Bin(n,prob1)","Bin(n,prob2)","Bin(n,prob3)","Bin(n,prob4)","Bin(n,prob5)","Bin(n,prob6)"),
       lty=c(1,1,1,1,1,1),lwd=c(3,3,3,3,3,3),col=c(6,5,4,3,2,1))

#Generate 100 random binomial variables
n<-100
x<-rbinom(n,1,prob2)
#Find the average x_bar
x_bar<-sum(x)/n
estP<-x_bar


#Plot both known distributions and their estimated distributions
p <-seq(0,n,by=1)
plot(p, dbinom(p,n,prob2),ylab="density",type="o",lwd=3,col=2,ylim=c(0,.2),xlim=c(0,20),main="Binomial Distributions")
lines(p, dbinom(p, n, estP),type="o",lwd=3,col=1)
legend(15,.15,c("Bin(n,prob1)","Bin(n,estP)"),lty=c(1,1),lwd=c(3,3),col=c(2,1))

