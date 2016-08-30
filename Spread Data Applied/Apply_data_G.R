### LINEAR GAUSSIAN STATE SPACE MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

#data located in "Spread_spx_main.R" for i=3,9,11,17,31,45 as follows:
# spreadin3; stockAin3; stockBin3
# spreadout3; stockAout3; stockBout3

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_G_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(spreadin3)-1; #N=N_theta=N_x
h=1; ci.l=0.025; ci.u=0.975; ess=0.3; magnification=10

run_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin3,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
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
ts.plot(magnification*spreadin3[(h+1):T],ylab="",main=paste("Gaussian Model with h=", h),ylim=c(-0.3,0.2))
lines(mean.y.pred_g,col="red",lty=3,main="h=")
lines(low.y.pred_g,col="green",lty=3)
lines(up.y.pred_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

ts.plot(p_g,ylab="Predictive log-likelihod")
dev.off()

### GENERATING NEEDED DATA (as above h=1; T=180; CI=95%)
#3 in-sample
runin3_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin3,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin3_g=runin3_g[[7]]
lowin3_g=runin3_g[[8]]
upin3_g=runin3_g[[9]]

#3 out-sample
runout3_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout3,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout3_g=runout3_g[[7]]
lowout3_g=runout3_g[[8]]
upout3_g=runout3_g[[9]]

###

#9 in-sample
runin9_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin9,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin9_g=runin9_g[[7]]
lowin9_g=runin9_g[[8]]
upin9_g=runin9_g[[9]]

#9 out-sample
runout9_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout9,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout9_g=runout9_g[[7]]
lowout9_g=runout9_g[[8]]
upout9_g=runout9_g[[9]]

###

#11 in-sample
runin11_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin11,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin11_g=runin11_g[[7]]
lowin11_g=runin11_g[[8]]
upin11_g=runin11_g[[9]]

#11 out-sample
runout11_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout11,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout11_g=runout11_g[[7]]
lowout11_g=runout11_g[[8]]
upout11_g=runout11_g[[9]]

###

#17 in-sample
runin17_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin17,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin17_g=runin17_g[[7]]
lowin17_g=runin17_g[[8]]
upin17_g=runin17_g[[9]]

#17 out-sample
runout17_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout17,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout17_g=runout17_g[[7]]
lowout17_g=runout17_g[[8]]
upout17_g=runout17_g[[9]]

###

#31 in-sample
runin31_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin31,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin31_g=runin31_g[[7]]
lowin31_g=runin31_g[[8]]
upin31_g=runin31_g[[9]]

#31 out-sample
runout31_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout31,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout31_g=runout31_g[[7]]
lowout31_g=runout31_g[[8]]
upout31_g=runout31_g[[9]]

###

#45 in-sample
runin45_g=smc2_pred_g(N=N,T=T,y=magnification*spreadin45,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin45_g=runin45_g[[7]]
lowin45_g=runin45_g[[8]]
upin45_g=runin45_g[[9]]

#45 out-sample
runout45_g=smc2_pred_g(N=N,T=T,y=magnification*spreadout45,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout45_g=runout45_g[[7]]
lowout45_g=runout45_g[[8]]
upout45_g=runout45_g[[9]]
