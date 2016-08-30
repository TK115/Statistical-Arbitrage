### LINEAR GAUSSIAN STATE SPACE MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_G_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(spreadout2uk)-1; #N=N_theta=N_x
h=1; ci.l=0.025; ci.u=0.975; ess=0.3; magnification=10

run_uk_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout2uk,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
rho_m_uk_g=run_uk_g[[1]]
sigma_m_uk_g=run_uk_g[[2]]
tau_m_uk_g=run_uk_g[[3]]
loglik_uk_g=run_uk_g[[4]]
mean.x_uk_g=run_uk_g[[6]]
mean.y.pred_uk_g=run_uk_g[[7]]
low.y.pred_uk_g=run_uk_g[[8]]
up.y.pred_uk_g=run_uk_g[[9]]
p_uk_g=run_uk_g[[10]]

#MCMC count
run_uk_g[[5]]/T

par(mfrow=c(2,2))
plot(rho_m_uk_g,ylab=expression(rho),xlab="Time",type="l")
plot(sigma_m_uk_g,ylab=expression(sigma),xlab="Time",type="l")
plot(tau_m_uk_g,ylab=expression(tau),xlab="Time",type="l")
plot(loglik_uk_g,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off()

#95% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout2uk[(h+1):T],ylab="",main=paste("LGM Model with h=", h),ylim=c(-0.1,0.2))
lines(mean.y.pred_uk_g,col="red",lty=3,main="h=")
lines(low.y.pred_uk_g,col="green",lty=3)
lines(up.y.pred_uk_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)
dev.off()

### GENERATING NEEDED DATA (as above i.e. h=1; T=180; CI=95%)
#2 out-sample
runout2uk_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout2uk,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout2uk_g=runout2uk_g[[7]]
lowout2uk_g=runout2uk_g[[8]]
upout2uk_g=runout2uk_g[[9]]

###

#8 out-sample
runout8uk_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout8uk,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout8uk_g=runout8uk_g[[7]]
lowout8uk_g=runout8uk_g[[8]]
upout8uk_g=runout8uk_g[[9]]

###

#12 out-sample
runout12uk_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout12uk,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout12uk_g=runout12uk_g[[7]]
lowout12uk_g=runout12uk_g[[8]]
upout12uk_g=runout12uk_g[[9]]

###

#16 out-sample
runout16uk_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout16uk,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout16uk_g=runout16uk_g[[7]]
lowout16uk_g=runout16uk_g[[8]]
upout16uk_g=runout16uk_g[[9]]

###

#20 out-sample
runout20uk_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout20uk,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout20uk_g=runout20uk_g[[7]]
lowout20uk_g=runout20uk_g[[8]]
upout20uk_g=runout20uk_g[[9]]

