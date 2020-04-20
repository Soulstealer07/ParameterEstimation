# The Pareto Distribution
install.packages("Pareto")
library(Pareto)
alpha1 <- 12
alpha2 <-9
alpha3 <- 7
alpha4 <- 10
alpha5 <- 15
alpha6 <- 20
beta1 <- 2
beta2<- .5
beta3<-.7
beta4<-2
beta5<-.1
beta6<-1
n<-5000
p = seq(0,n,by=1)
plot(p,dPareto(p,alpha1,beta1),ylab="density",type="l",lwd=3,col=6,xlim=c(0,4000),ylim=c(0,.0015),main="Pareto Distributions")
lines(p,dPareto(p,alpha2,beta2),type="l",lwd=3,col=5)
lines(p,dPareto(p, alpha3, beta3),type="l",lwd=3,col=4)
lines(p,dPareto(p,alpha4,beta4),type="l",lwd=3,col=3)
lines(p,dPareto(p,alpha5,beta5),type="l",lwd=3,col=2)
lines(p,dPareto(p,alpha6,beta6),type="l",lwd=3,col=1)
legend(2000,.0015,c("Pareto(alpha1,beta1)","Pareto(alpha2,beta2)","Pareto(alpha3,beta3)","Pareto(alpha4,beta4)","Pareto(alpha5,beta5)","Pareto(alpha6,beta6)"),
       lty=c(1,1,1,1,1,1),lwd=c(3,3,3,3,3,3),col=c(6,5,4,3,2,1))


# Generate 100 random Pareto variables 
n<-100
alpha<-3
beta<-.3
x<-rPareto(n,alpha,beta)
#Find the minimum statistic in the random sample
alpha_est <- min(x)

#Find estimate for Beta

log <- log(x/alpha_est)
sum_log<-sum(log)
beta_est <- n/sum_log


#Plot both known distributions and their estimated distributions
p = seq(alpha+1,n,by=1)
plot(p,dPareto(p,alpha,beta),ylab="density",type="l",xlim=c(alpha,100),ylim=c(0,.07),lwd=3,col=2,main="Pareto vs Estimated Pareto")
lines(p,dPareto(p, alpha_est, beta_est),type="l",lwd=3,col=1)
legend(20,.04,c("Pareto(alpha,beta)","Pareto(alpha_est,beta_est)"),
              lty=c(1,2),lwd=c(3,3),col=c(2,1))
