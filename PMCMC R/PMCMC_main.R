install.packages("tseries")
install.packages("fUnitRoots")
install.packages("quantmod")
install.packages("Quandl")
install.packages("crch")
install.packages("pscl")
install.packages("truncnorm")
install.packages("coda")
library(tseries) #adf.test; kpss.test
library(fUnitRoots) #adfTest
library(quantmod)
library(Quandl)
library(crch)
library(pscl)
library(truncnorm)
library(coda)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Cointegration")
source("Cointegration_functions.R")

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/PMCMC R")
source("PMCMC_functions.R")

rm(list=ls())
#set.seed(128)

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

# IBM.close/MSFT.close i=10
i=10
spread2=create.spread.opt(IBM.close[(170*(i-1)):(170*i)],MSFT.close[(170*(i-1)):(170*i)],0.8,0.05)[[2]]
y2=ROC(spread2, type='discrete', na.pad=FALSE)

# AAPL.close/MSFT.close i=7
i=7
spread3=create.spread.opt(AAPL.close[(170*(i-1)):(170*i)],MSFT.close[(170*(i-1)):(170*i)],0.8,0.05)[[2]]
y3=ROC(spread3, type='discrete', na.pad=FALSE)

#################################################################
sigma=0.8; N=100; df=2; nsteps=25000

ptm <- proc.time()
gmhp_run=gmhp(rho_0=0,tau_0=0.8,sigma_rho=2.5,sigma_tau=0.15,nsteps=nsteps,y=spread2)
proc.time() - ptm

Rho.p=gmhp_run[[1]][-(1:(floor(nsteps/10)))]
Tau.p=gmhp_run[[2]][-(1:(floor(nsteps/10)))]

par(mfrow=c(2,1))
hist(Rho.p,breaks=50,main="Histogram of rho with N=100",xlab="rho")
hist(Tau.p,breaks=50,main="Histogram of tau with N=100",xlab="tau")

### Diagnostics
summary(as.mcmc(Rho.p))
summary(as.mcmc(Tau.p))

par(mfrow=c(2,2))
ts.plot(Rho.p,main="Trace Plot",ylab=expression(rho))
acf(Rho.p,main=expression(rho))
ts.plot(Tau.p,main="Trace Plot",ylab=expression(tau))
acf(Tau.p,main=expression(tau))

# effective sample size (proportion)
effectiveSize(mcmc(Rho.p))/nsteps #0.32
effectiveSize(mcmc(Tau.p))/nsteps #0.159
# rejection probability
1-rejectionRate(mcmc(Rho.p)) #0.31
1-rejectionRate(mcmc(Tau.p)) #0.22

### model with rho, tau, sigma
df=3; N=100; nsteps=10000
ptm <- proc.time()
gmhp_run_full=gmhp.full(rho_0=0,tau_0=0.2,sigma_0=0.2,sigma_rho=0.3,sigma_tau=0.5,sigma_sigma=0.5,nsteps=nsteps,y=spread2)
proc.time() - ptm

rho.p=gmhp_run_full[[1]][-(1:(floor(nsteps/10)))]
tau.p=gmhp_run_full[[2]][-(1:(floor(nsteps/10)))]
sigma.p=gmhp_run_full[[3]][-(1:(floor(nsteps/10)))]

par(mfrow=c(3,1))
hist(rho.p,breaks=50,main="Histogram of rho with N=100",xlab="rho")
hist(tau.p,breaks=50,main="Histogram of tau with N=100",xlab="tau")
hist(sigma.p,breaks=50,main="Histogram of sigma with N=100",xlab="sigma")

### Diagnostics
summary(as.mcmc(rho.p))
summary(as.mcmc(tau.p))
summary(as.mcmc(sigma.p))

par(mfrow=c(3,2))
ts.plot(rho.p,main="Trace Plot",ylab=expression(rho))
acf(rho.p,main=expression(rho))
ts.plot(tau.p,main="Trace Plot",ylab=expression(tau))
acf(tau.p,main=expression(tau))
ts.plot(sigma.p,main="Trace Plot",ylab=expression(sigma))
acf(sigma.p,main=expression(sigma))

# effective sample size (proportion)
effectiveSize(mcmc(rho.p))/nsteps #0.17
effectiveSize(mcmc(tau.p))/nsteps #0.14
effectiveSize(mcmc(sigma.p))/nsteps #0.13

# acceptance probability
1-rejectionRate(mcmc(rho.p)) #0.24
1-rejectionRate(mcmc(tau.p)) #0.22
1-rejectionRate(mcmc(sigma.p)) #0.22
