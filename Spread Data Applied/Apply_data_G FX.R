### LINEAR Gaussian-INNOVATIONS MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

#NZDUSD - ZARUSD out spread for years 1999,2000,2002,2007,2009,2012

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_G_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(NZDUSD_ZARUSD1999out)-1; #N=N_theta=N_x
h=1; ci.l=0.025; ci.u=0.975; ess=0.3; magnification=1

run_g=smc2_pred_g(N=N,T=T,y=NZDUSD_ZARUSD1999out*magnification,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
rho_m_g=run_g[[1]]
sigma_m_g=run_g[[2]]
tau_m_g=run_g[[3]]
loglik_g=run_g[[4]]
mean.x_g=run_g[[6]]
mean.y.pred_g=run_g[[7]]
low.y.pred_g=run_g[[8]]
up.y.pred_g=run_g[[9]]
p_g=run_g[[10]]

#MCMC count
run_g[[5]]/T

par(mfrow=c(2,2))
plot(rho_m_g,ylab=expression(rho),xlab="Time",type="l")
plot(sigma_m_g,ylab=expression(sigma),xlab="Time",type="l",ylim=c(0,0.02))
plot(tau_m_g,ylab=expression(tau),xlab="Time",type="l",ylim=c(0,0.03))
plot(loglik_g,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off() 

#95% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
#par(mfrow=c(2,1))
ts.plot(magnification*NZDUSD_ZARUSD1999out[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.3,0.2))
lines(mean.y.pred_g,col="red",lty=3,main="h=")
lines(low.y.pred_g,col="green",lty=3)
lines(up.y.pred_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

ts.plot(p_g,ylab="Predictive log-likelihod")
dev.off()

### GENERATING NEEDED DATA (as above i.e. h=1; T=129; CI=95%)
#1999 out-sample
runout99_g=smc2_pred_g(N=N,T=T,y=magnification*NZDUSD_ZARUSD1999out,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout99_g=runout99_g[[7]]
lowout99_g=runout99_g[[8]]
upout99_g=runout99_g[[9]]

###
#2000 out-sample
runout00_g=smc2_pred_g(N=N,T=T,y=magnification*NZDUSD_ZARUSD2000out,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout00_g=runout00_g[[7]]
lowout00_g=runout00_g[[8]]
upout00_g=runout00_g[[9]]

###
#2002 out-sample
runout02_g=smc2_pred_g(N=N,T=T,y=magnification*NZDUSD_ZARUSD2002out,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout02_g=runout02_g[[7]]
lowout02_g=runout02_g[[8]]
upout02_g=runout02_g[[9]]

###
#2007 out-sample
runout07_g=smc2_pred_g(N=N,T=T,y=magnification*NZDUSD_ZARUSD2007out,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout07_g=runout07_g[[7]]
lowout07_g=runout07_g[[8]]
upout07_g=runout07_g[[9]]

###
#2009 out-sample
runout09_g=smc2_pred_g(N=N,T=T,y=magnification*NZDUSD_ZARUSD2009out,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout09_g=runout09_g[[7]]
lowout09_g=runout09_g[[8]]
upout09_g=runout09_g[[9]]

###
#2012 out-sample
runout12_g=smc2_pred_g(N=N,T=T,y=magnification*NZDUSD_ZARUSD2012out,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout12_g=runout12_g[[7]]
lowout12_g=runout12_g[[8]]
upout12_g=runout12_g[[9]]
