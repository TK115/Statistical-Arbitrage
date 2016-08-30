setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/PMCMC R")
#install.packages("truncnorm")
#install.packages("pscl")
#install.packages("coda")
#install.packages("doParallel")
library(truncnorm)
library(pscl)
library(coda)
library(doParallel)
source("PMCMC_functions.R")

nsteps=25000; N=1000; df=4; magnification=1

ptm <- proc.time()
test=gmhp.full(rho_0=0,tau_0=0.1,sigma_0=0.1,sigma_rho=0.05,sigma_tau=0.15,sigma_sigma=0.1,nsteps=nsteps,y=magnification*sp3out)
proc.time() - ptm

#remove burn-in
test.rho=test[[1]][-(1:(floor(nsteps/10)))]
test.tau=test[[2]][-(1:(floor(nsteps/10)))]
test.sigma=test[[3]][-(1:(floor(nsteps/10)))]
loglik=log(test[[4]][1:(floor(nsteps/10))])

# acceptance probability
1-rejectionRate(mcmc(test.rho))
1-rejectionRate(mcmc(test.tau))
1-rejectionRate(mcmc(test.sigma))

par(mfrow=c(2,2))
hist(test.rho,breaks=50,main="Histogram of rho with N=1000",xlab="Rho")
hist(test.tau,breaks=50,main="Histogram of tau with N=1000",xlab="Tau")
hist(test.sigma,breaks=50,main="Histogram of sigma with N=1000",xlab="Sigma")
plot(loglik,main="Burn-in phase",xlab="Iterations",ylab="log-likelihood",type="l")

### Diagnostics
summary(as.mcmc(test.rho))
summary(as.mcmc(test.tau))
summary(as.mcmc(test.sigma))

par(mfrow=c(3,2))
ts.plot(test.rho,main="Trace Plot",ylab=expression(rho))
acf(test.rho,main=expression(rho))
ts.plot(test.tau,main="Trace Plot",ylab=expression(tau))
acf(test.tau,main=expression(tau))
ts.plot(test.sigma,main="Trace Plot",ylab=expression(sigma))
acf(test.sigma,main=expression(sigma))

# effective sample size (proportion)
effectiveSize(mcmc(test.rho))/nsteps
effectiveSize(mcmc(test.tau))/nsteps
effectiveSize(mcmc(test.sigma))/nsteps