### Stochastic Volatility MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

#data located in "Spread_spx_main.R" for i=3,9,11,17,31,45 as follows:
# spreadin3; stockAin3; stockBin3
# spreadout3; stockAout3; stockBout3

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_SV_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(spreadout3)-1; #N=N_theta=N_x
h=1; ci.l=0.025; ci.u=0.975; ess=0.3; magnification=1

run_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout3,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
rho_m_sv=run_sv[[1]]
sigma_m_sv=run_sv[[2]]
tau_m_sv=run_sv[[3]]
loglik_sv=run_sv[[4]]
mean.x_sv=run_sv[[6]]
mean.y.pred_sv=run_sv[[7]]
low.y.pred_sv=run_sv[[8]]
up.y.pred_sv=run_sv[[9]]
p_sv=run_sv[[10]]

#MCMC count
run_sv[[5]]/T

par(mfrow=c(2,2))
plot(rho_m_sv,ylab=expression(rho),xlab="Time",type="l")
plot(sigma_m_sv,ylab=expression(sigma),xlab="Time",type="l")
plot(tau_m_sv,ylab=expression(tau),xlab="Time",type="l")
plot(loglik_sv,ylab=expression(logp[y[0:t]]),xlab="Time",type="l")
dev.off()

#95% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout3[(h+1):T],ylab="",main=paste("SV Model with h=", h),ylim=c(-0.1,0.1))
lines(mean.y.pred_sv,col="red",lty=3,main="h=")
lines(low.y.pred_sv,col="green",lty=3)
lines(up.y.pred_sv,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)
dev.off()

### GENERATING NEEDED DATA (as above i.e. h=1; T=180; CI=95%)
#3 out-sample
runout3_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout3,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout3_sv=runout3_sv[[7]]
lowout3_sv=runout3_sv[[8]]
upout3_sv=runout3_sv[[9]]

###

#9 out-sample
runout9_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout9,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout9_sv=runout9_sv[[7]]
lowout9_sv=runout9_sv[[8]]
upout9_sv=runout9_sv[[9]]

###

#11 out-sample
runout11_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout11,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout11_sv=runout11_sv[[7]]
lowout11_sv=runout11_sv[[8]]
upout11_sv=runout11_sv[[9]]

###

#17 out-sample
runout17_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout17,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout17_sv=runout17_sv[[7]]
lowout17_sv=runout17_sv[[8]]
upout17_sv=runout17_sv[[9]]

###

#31 out-sample
runout31_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout31,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout31_sv=runout31_sv[[7]]
lowout31_sv=runout31_sv[[8]]
upout31_sv=runout31_sv[[9]]

###

#45 out-sample
runout45_sv=smc2_pred_sv(N=N,T=T,y=magnification*spreadout45,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout45_sv=runout45_sv[[7]]
lowout45_sv=runout45_sv[[8]]
upout45_sv=runout45_sv[[9]]
