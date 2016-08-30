### LINEAR t-INNOVATIONS MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

#data located in "Spread_spx_main.R" for i=3,9,11,17,31,45 as follows:
# spreadin3; stockAin3; stockBin3
# spreadout3; stockAout3; stockBout3

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(spreadout3)-1; df=6 #N=N_theta=N_x
h=1; ci.l=0.025; ci.u=0.975; ess=0.3; magnification=10

run=smc2_pred(N=N,T=T,y=spreadout3*magnification,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
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
ts.plot(magnification*spreadout3[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.3,0.2))
lines(mean.y.pred,col="red",lty=3,main="h=")
lines(low.y.pred,col="green",lty=3)
lines(up.y.pred,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

ts.plot(p_t,ylab="Predictive log-likelihod")
dev.off()

### GENERATING NEEDED DATA (as above i.e. df=6; h=1; T=180; CI=95%)
#3 in-sample
runin3=smc2_pred(N=N,T=T,y=magnification*spreadin3,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin3=runin3[[7]]
lowin3=runin3[[8]]
upin3=runin3[[9]]

#3 out-sample
runout3=smc2_pred(N=N,T=T,y=magnification*spreadout3,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout3=runout3[[7]]
lowout3=runout3[[8]]
upout3=runout3[[9]]

###

#9 in-sample
runin9=smc2_pred(N=N,T=T,y=magnification*spreadin9,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin9=runin9[[7]]
lowin9=runin9[[8]]
upin9=runin9[[9]]

#9 out-sample
runout9=smc2_pred(N=N,T=T,y=magnification*spreadout9,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout9=runout9[[7]]
lowout9=runout9[[8]]
upout9=runout9[[9]]

###

#11 in-sample
runin11=smc2_pred(N=N,T=T,y=magnification*spreadin11,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin11=runin11[[7]]
lowin11=runin11[[8]]
upin11=runin11[[9]]

#11 out-sample
runout11=smc2_pred(N=N,T=T,y=magnification*spreadout11,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout11=runout11[[7]]
lowout11=runout11[[8]]
upout11=runout11[[9]]

###

#17 in-sample
runin17=smc2_pred(N=N,T=T,y=magnification*spreadin17,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin17=runin17[[7]]
lowin17=runin17[[8]]
upin17=runin17[[9]]

#17 out-sample
runout17=smc2_pred(N=N,T=T,y=magnification*spreadout17,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout17=runout17[[7]]
lowout17=runout17[[8]]
upout17=runout17[[9]]

###

#31 in-sample
runin31=smc2_pred(N=N,T=T,y=magnification*spreadin31,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin31=runin31[[7]]
lowin31=runin31[[8]]
upin31=runin31[[9]]

#31 out-sample
runout31=smc2_pred(N=N,T=T,y=magnification*spreadout31,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout31=runout31[[7]]
lowout31=runout31[[8]]
upout31=runout31[[9]]

###

#45 in-sample
runin45=smc2_pred(N=N,T=T,y=magnification*spreadin45,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin45=runin45[[7]]
lowin45=runin45[[8]]
upin45=runin45[[9]]

#45 out-sample
runout45=smc2_pred(N=N,T=T,y=magnification*spreadout45,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout45=runout45[[7]]
lowout45=runout45[[8]]
upout45=runout45[[9]]
