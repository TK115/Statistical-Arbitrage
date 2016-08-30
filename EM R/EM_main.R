install.packages("quantmod")
library(quantmod)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/EM R")

rm(list=ls())
#set.seed(128)
source("Cointegration_functions.R")
source("EM_functions.R")

AAPL<-Quandl("WIKI/AAPL")
IBM<-Quandl("WIKI/IBM")
MSFT<-Quandl("WIKI/MSFT")
AAPL.close<-AAPL$Close
IBM.close<-IBM$Close
MSFT.close<-MSFT$Close

# take stationary spreads
# IBM.close/AAPL.close i=11
i=11
spread1=create.spread.opt(IBM.close[(170*(i-1)):(170*i)],AAPL.close[(170*(i-1)):(170*i)],0.8,0.01)[[2]]
y1=ROC(spread1, type='discrete', na.pad=FALSE)

for(i in 1:length(y1)){
	if(dnorm(y1[i])<1e-20) y1[i]=y1[i-1]
}

# IBM.close/MSFT.close i=10
i=10
spread2=create.spread.opt(IBM.close[(170*(i-1)):(170*i)],MSFT.close[(170*(i-1)):(170*i)],0.8,0.05)[[2]]
y2=ROC(spread2, type='discrete', na.pad=FALSE)

for(i in 1:length(y2)){
	if(dnorm(y2[i])<1e-20) y2[i]=y2[i-1]
}

# AAPL.close/MSFT.close i=7
i=7
spread3=create.spread.opt(AAPL.close[(170*(i-1)):(170*i)],MSFT.close[(170*(i-1)):(170*i)],0.8,0.05)[[2]]
y3=ROC(spread3, type='discrete', na.pad=FALSE)

for(i in 1:length(y3)){
	if(dnorm(y3[i])<1e-20) y3[i]=y3[i-1]
}

####################################################################
m=500; rho=-0.01; tau=0.5; sigma=1.5; N=1000
z1=vector("numeric"); z2=vector("numeric"); z3=vector("numeric"); logp=vector("numeric")
z1[1]=rho; z2[1]=tau; z3[1]=sigma

for(k in 1:m){
	bpf.sir=bpf_sir(N=N,y=spread3,T=length(spread3)-1,rho=z1[k],tau=z2[k],sigma=z3[k])
	x.est=bpf.sir[[1]]
	logp[k]=bpf.sir[[2]]
	s=S(y1,x.est,1)
	s1=mean(rowSums(s[[1]],na.rm=TRUE))
	s2=mean(rowSums(s[[2]],na.rm=TRUE))
	s3=mean(rowSums(s[[3]],na.rm=TRUE))
	s4=mean(rowSums(s[[4]],na.rm=TRUE))
	z1[k+1]=s3/s2
	z2[k+1]=sqrt(s4/(T+1)-s3^2/(s2*(T+1)))
	z3[k+1]=sqrt(s1/(T+1))
}

par(mfrow=c(3,1))
plot(z1,type="l",ylab=expression(rho),xlab="Iteration", ylim=c(-0.5,0.5),main="Particle Approximation of rho with N=1000")
plot(z2,type="l",ylab=expression(tau),xlab="Iteration", ylim=c(0,1.5),main="Particle Approximation of tau with N=1000")
plot(z3,type="l",ylab=expression(sigma),xlab="Iteration", ylim=c(0,2),main="Particle Approximation of sigma with N=1000")
dev.off()

# log_p
ts.plot(logp,ylab=expression(log(p[theta[k]](y[0:T]))),xlab="Iteration",main="Particle Approximation of log(p) with N=1000")