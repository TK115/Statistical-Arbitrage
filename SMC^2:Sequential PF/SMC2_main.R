### LINEAR t-INNOVATIONS MODEL
rm(list=ls())
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/SMC^2:Sequential PF")
source("SMC2_functions.R")

rho=0.8; tau=1; sigma=0.5; T=100; df=2 #0<df<1 possible

### Calculate y_[0:T] and trajectory x*_[0:T]
x=vector("numeric")
y=vector("numeric")
x[1]=rt(1,df=df)
for(n in 1:(T+1)){
	x[n+1]=rho*x[n]+tau*rt(1,df=df)
	y[n]=x[n]+sigma*rt(1,df=df)
}

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1 #N=N_theta=N_x
run=smc2(N=N,T=T,y=y,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma)
rho_m=run[[1]]
sigma_m=run[[2]]
tau_m=run[[3]]
loglik=run[[4]]
mean.x=run[[6]]
low.x=run[[7]]
up.x=run[[8]]
p=run[[9]]

#MCMC count ratio
run[[5]]/T

par(mfrow=c(2,2))
plot(rho_m,ylab=expression(rho),xlab="Time",type="l",ylim=c(0,1))
abline(h=0.8,col="red")
plot(sigma_m,ylab=expression(sigma),xlab="Time",type="l",ylim=c(0,3))
abline(h=0.5,col="red")
plot(tau_m,ylab=expression(tau),xlab="Time",type="l",ylim=c(0,3))
abline(h=1,col="red")
plot(loglik,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off()

#95% credible interval
par(mfrow=c(2,1))
ts.plot(y,ylab="")
lines(mean.x,col="red",lty=3)
lines(low.x,col="green",lty=3)
lines(up.x,col="green",lty=3)
legend("topleft",c("y_t","E(x_t|y_0:t)","95% CI of (x_t|y_0:t)"),lty=c(1,3,3,3),col=c("black","red","green","green"))
ts.plot(p,ylab="Predictive log-likelihod")
dev.off()

#######################################################################################
### PREDICTIVE INFERENCE
N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1 #N=N_theta=N_x
h=1
run2=smc2_pred(N=N,T=T,y=y,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h)
rho_m2=run2[[1]]
sigma_m2=run2[[2]]
tau_m2=run2[[3]]
loglik2=run2[[4]]
mean.x2=run2[[6]]
mean.y.pred=run2[[7]]
low.y.pred=run2[[8]]
up.y.pred=run2[[9]]
p2=run2[[10]]

#MCMC count
run2[[5]]/T

par(mfrow=c(2,2))
plot(rho_m2,ylab=expression(rho),xlab="Time",type="l",ylim=c(0,1))
abline(h=0.8,col="red")
plot(sigma_m2,ylab=expression(sigma),xlab="Time",type="l",ylim=c(0,3))
abline(h=0.5,col="red")
plot(tau_m2,ylab=expression(tau),xlab="Time",type="l",ylim=c(0,3))
abline(h=1,col="red")
plot(loglik2,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off()

#90% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
par(mfrow=c(2,1))
ts.plot(y[(h+1):T],ylab="",ylim=c(-20,20),main=paste("t Model with h=", h))
lines(mean.y.pred,col="red",lty=3,main="h=")
lines(low.y.pred,col="green",lty=3)
lines(up.y.pred,col="green",lty=3)
legend("topleft",c("y_t","E(y_t+h|y_0:t)","90% CI of (y_t+h|y_0:t)"),lty=c(1,3,3,3),col=c("black","red","green","green"))
ts.plot(p2,ylab="Predictive log-likelihod")
dev.off()
