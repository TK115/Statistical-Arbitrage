install.packages("R.matlab")
library(R.matlab)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/PMCMC Matlab")

### EXAMPLE
rm(list=ls())
rho=0.8; tau=1; sigma=0.1; T=500

x=vector("numeric")
y=vector("numeric")
x[1]=rnorm(1)
for(n in 1:(T+1)){ #
	x[n+1]=rho*x[n]+tau*rnorm(1)
	y[n]=x[n]+sigma*rnorm(1)
}

x=x[1:length(y)]

data=data.frame(y,x)
writeMat("data.mat", labpcexport = data)