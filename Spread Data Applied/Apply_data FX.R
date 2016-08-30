### LINEAR t-INNOVATIONS MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

#NZDUSD - ZARUSD out spread for years 1999,2000,2002,2007,2009,2012

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(NZDUSD_ZARUSD1999out)-1; df=6 #N=N_theta=N_x
h=1; ci.l=0.025; ci.u=0.975; ess=0.3; magnification=1

run=smc2_pred(N=N,T=T,y=NZDUSD_ZARUSD1999out*magnification,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
rho_m=run[[1]]
sigma_m=run[[2]]
tau_m=run[[3]]
loglik=run[[4]]
mean.x=run[[6]]
mean.y.pred=run[[7]]
low.y.pred=run[[8]]
up.y.pred=run[[9]]
p_t=run[[10]]

#MCMC count
run[[5]]/T

par(mfrow=c(2,2))
plot(rho_m,ylab=expression(rho),xlab="Time",type="l")
plot(sigma_m,ylab=expression(sigma),xlab="Time",type="l",ylim=c(0,0.02))
plot(tau_m,ylab=expression(tau),xlab="Time",type="l",ylim=c(0,0.03))
plot(loglik,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off() 

#95% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
#par(mfrow=c(2,1))
ts.plot(magnification*NZDUSD_ZARUSD1999out[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.3,0.2))
lines(mean.y.pred,col="red",lty=3,main="h=")
lines(low.y.pred,col="green",lty=3)
lines(up.y.pred,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

ts.plot(p_t,ylab="Predictive log-likelihod")
dev.off()

### GENERATING NEEDED DATA (as above i.e. df=6; h=1; T=129; CI=95%)
#1999 out-sample
runout99=smc2_pred(N=N,T=T,y=magnification*NZDUSD_ZARUSD1999out,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout99=runout99[[7]]
lowout99=runout99[[8]]
upout99=runout99[[9]]

###
#2000 out-sample
runout00=smc2_pred(N=N,T=T,y=magnification*NZDUSD_ZARUSD2000out,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout00=runout00[[7]]
lowout00=runout00[[8]]
upout00=runout00[[9]]

###
#2002 out-sample
runout02=smc2_pred(N=N,T=T,y=magnification*NZDUSD_ZARUSD2002out,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout02=runout02[[7]]
lowout02=runout02[[8]]
upout02=runout02[[9]]

###
#2007 out-sample
runout07=smc2_pred(N=N,T=T,y=magnification*NZDUSD_ZARUSD2007out,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout07=runout07[[7]]
lowout07=runout07[[8]]
upout07=runout07[[9]]

###
#2009 out-sample
runout09=smc2_pred(N=N,T=T,y=magnification*NZDUSD_ZARUSD2009out,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout09=runout09[[7]]
lowout09=runout09[[8]]
upout09=runout09[[9]]

###
#2012 out-sample
runout12=smc2_pred(N=N,T=T,y=magnification*NZDUSD_ZARUSD2012out,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout12=runout12[[7]]
lowout12=runout12[[8]]
upout12=runout12[[9]]
