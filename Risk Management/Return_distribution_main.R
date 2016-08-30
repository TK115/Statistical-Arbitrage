### LINEAR t-INNOVATIONS MODEL
#set.seed(42)
rm(list=ls())
library(pscl)
library(truncnorm)
library(coda)
library(tseries)
library(TTR)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_functions_spread_data.R")

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("Trading_strategy_functions.R")

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Risk Management")
source("Return_distribution_functions.R")

rho=0.9; tau=0.025; sigma=0.01; df=6; T=252; sig_WN=0.07
N=10000; sigma_rho=0.05; sigma_tau=0.15; sigma_sigma=0.1
h=1; ci.l=0.025; ci.u=0.975; os=2; cs=0.75; ol=-2; cl=-0.75
risk=-0.5; exit_short=3; exit_long=-3; ess=0.33

ts.plot(scenario.generator(T),ylim=c(-0.2,0.2),xlab="Time (in days)",ylab="Logarithmic Price of Currency",main="Seven Sample Spreads")
lines(scenario.generator(T),col=2)
lines(scenario.generator(T),col=3)
lines(scenario.generator(T),col=4)
lines(scenario.generator(T),col=5)
lines(scenario.generator(T),col=6)
lines(scenario.generator(T),col=7)

run=strategy.return.dist(n=10000,type='realistic')
return=run[[1]]
mean(return); sd(return)
return2=run[[2]]
mean(return2); sd(return2)
return.z=run[[3]]
mean(return.z); sd(return.z)
return_risk=run[[4]]
mean(return_risk); sd(return_risk)
return2_risk=run[[5]]
mean(return2_risk); sd(return2_risk)
return.z_risk=run[[6]]
mean(return.z_risk); sd(return.z_risk)
ADF=run[[7]]
mean(ADF); sd(ADF)

par(mfrow=c(2,3))
hist(return,breaks=50,main="Returns with Mean-Reversion Strategy t-Model")
hist(return2,breaks=50,main="Returns with Band-Reversion Strategy t-Model")
hist(return.z,breaks=50,main="Returns with Z-Score Strategy t-Model")
hist(return_risk,breaks=50,main="Returns with Mean-Reversion Strategy t-Model")
hist(return2_risk,breaks=50,main="Returns with Mean-Reversion Strategy t-Model")
hist(return_risk,breaks=50,main="Returns with Risk Mean-Reversion Strategy t-Model")

### Bollinger Bands
run.bb=strategy.return.dist.bb(n=10000,type='realistic')
return.bb=run.bb[[1]]
mean(return.bb); sd(return.bb)
return2.bb=run.bb[[2]]
mean(return2.bb); sd(return2.bb)
return.z.bb=run.bb[[3]]
mean(return.z.bb); sd(return.z.bb)
return_risk.bb=run.bb[[4]]
mean(return_risk.bb); sd(return_risk.bb)
return2_risk.bb=run.bb[[5]]
mean(return2_risk.bb); sd(return2_risk.bb)
return.z_risk.bb=run.bb[[6]]
mean(return.z_risk.bb); sd(return.z_risk.bb)
ADF.bb=run.bb[[7]]
mean(ADF.bb); sd(ADF.bb)

par(mfrow=c(2,2))
hist(return.bb,breaks=50,main="Returns with Mean-Reversion Strategy BB",xlab="Cumulative Returns",xlim=c(-0.9,1))
hist(return2.bb,breaks=50,main="Returns with Band-Reversion Strategy BB",xlab="Cumulative Returns",xlim=c(-0.9,2))
hist(return_risk.bb,breaks=50,main="Returns with Risk Mean-Reversion Strategy BB",xlab="Cumulative Returns")
hist(return2_risk.bb,breaks=50,main="Returns with Risk Band-Reversion Strategy BB",xlab="Cumulative Returns")
dev.off()

par(mfrow=c(1,2))
hist(return.z.bb,breaks=50,main="Returns with Z-Score Strategy",xlab="Cumulative Returns",xlim=c(-0.9,1))
hist(return.z_risk.bb,breaks=50,main="Returns with Risk Z-Score Strategy",xlab="Cumulative Returns")
