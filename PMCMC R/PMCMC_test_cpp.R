rm(list=ls())
setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/PMCMC R")
library(coda)
library(doParallel)
library(Rcpp)
 
detectCores()
getDoParWorkers()
cl=makeCluster(4)
registerDoParallel(cl)

set.seed(128)
rho=0.8; tau=1; sigma=0.5; T=250; df=2

### Calculate y_[0:T] and trajectory x*_[0:T]
x=vector("numeric")
y=vector("numeric")
x[1]=rt(1,df=df)
for(n in 1:(T+1)){
  x[n+1]=rho*x[n]+tau*rt(1,df=df)
  y[n]=x[n]+sigma*rt(1,df=df)
}

rho_0=0.5; tau_0=0.7; sigma_0=0.8; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; nsteps=2000; N=1000
test=gmhpfull(rho_0,tau_0,sigma_0,sigma_rho,sigma_tau,sigma_sigma,nsteps,y,df,N,T);

#remove burn-in
test.rho=test[[1]][-(1:(floor(nsteps/10)))]
test.tau=test[[2]][-(1:(floor(nsteps/10)))]
test.sigma=test[[3]][-(1:(floor(nsteps/10)))]
loglik=log(test[[4]][1:(floor(nsteps/10))])

# acceptance probability
1-rejectionRate(mcmc(test.rho)) #0.17
1-rejectionRate(mcmc(test.tau)) #0.27
1-rejectionRate(mcmc(test.sigma)) #0.26

par(mfrow=c(2,2))
hist(test.rho,breaks=50,main="Histogram of rho with N=1000",xlab="Rho")
hist(test.tau,breaks=50,main="Histogram of tau with N=1000",xlab="Tau")
hist(test.sigma,breaks=50,main="Histogram of sigma with N=1000",xlab="Sigma")
plot(loglik,main="Burn-in phase",xlab="Iterations",ylab="log-likelihood",type="l")

### Diagnostics
summary(as.mcmc(test.rho)) #mean 0.80
summary(as.mcmc(test.tau)) #mean 0.92
summary(as.mcmc(test.sigma)) #mean 0.45

par(mfrow=c(3,2))
ts.plot(test.rho,main="Trace Plot",ylab=expression(rho))
acf(test.rho,main=expression(rho))
ts.plot(test.tau,main="Trace Plot",ylab=expression(tau))
acf(test.tau,main=expression(tau))
ts.plot(test.sigma,main="Trace Plot",ylab=expression(sigma))
acf(test.sigma,main=expression(sigma))

# effective sample size (proportion)
effectiveSize(mcmc(test.rho))/nsteps #0.102
effectiveSize(mcmc(test.tau))/nsteps #0.081
effectiveSize(mcmc(test.sigma))/nsteps #0.073