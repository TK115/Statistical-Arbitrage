### LINEAR GAUSSIAN-INNOVATIONS MODEL
rm(list=ls())
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/SMC^2:Sequential PF")
source("SMC2_G_functions.R")

rho=0.8; tau=1; sigma=0.5; T=100

### Calculate y_[0:T] and trajectory x*_[0:T]
xg=vector("numeric")
yg=vector("numeric")
xg[1]=rnorm(1)
for(n in 1:(T+1)){
	xg[n+1]=rho*xg[n]+tau*rnorm(1)
	yg[n]=xg[n]+sigma*rnorm(1)
}

### PREDICTIVE INFERENCE
N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1 #N=N_theta=N_x
h=1
run_g=smc2_pred_g(N=N,T=T,y=yg,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h)
rho_mg=run_g[[1]]
sigma_mg=run_g[[2]]
tau_mg=run_g[[3]]
loglikg=run_g[[4]]
mean.xg=run_g[[6]]
mean.y.pred_g=run_g[[7]]
low.y.pred_g=run_g[[8]]
up.y.pred_g=run_g[[9]]
pg=run_g[[10]]

#MCMC count
run_g[[5]]/T

par(mfrow=c(2,2))
plot(rho_mg,ylab=expression(rho),xlab="Time",type="l",ylim=c(0,1))
abline(h=0.8,col="red")
plot(sigma_mg,ylab=expression(sigma),xlab="Time",type="l",ylim=c(0,3))
abline(h=0.5,col="red")
plot(tau_mg,ylab=expression(tau),xlab="Time",type="l",ylim=c(0,3))
abline(h=1,col="red")
plot(loglikg,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off()

#90% credible prediction interval of (y_t+h|y_0:t)
par(mfrow=c(2,1))
ts.plot(yg,ylab="",ylim=c(-20,20),main=paste("GLM with h=", h))
lines(mean.y.pred_g,col="red",lty=3,main="h=")
lines(low.y.pred_g,col="green",lty=3)
lines(up.y.pred_g,col="green",lty=3)
legend("topleft",c("y_t","E(y_t+h|y_0:t)","90% CI of (y_t+h|y_0:t)"),lty=c(1,3,3,3),col=c("black","red","green","green"))
ts.plot(pg,ylab="Predictive log-likelihod")
dev.off()