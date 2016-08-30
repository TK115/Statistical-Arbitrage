### LINEAR t-INNOVATIONS MODEL
#set.seed(42)
library(pscl)
library(truncnorm)
library(coda)

#data located in "Spread_spx_main.R" for i=2,8,12,16,20 as follows:
# spreadin2uk; stockAin2uk; stockBin2uk
# spreadoutuk; stockAout2uk; stockBout2uk

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("SMC2_functions_spread_data.R")

N=10000; sigma_rho=0.1; sigma_tau=0.1; sigma_sigma=0.1; T=length(spreadin2uk)-1; df=5 #N=N_theta=N_x
h=1; ci.l=0.05; ci.u=0.95; ess=0.3; magnification=1

run=smc2_pred(N=N,T=T,y=spreadin2uk*magnification,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
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

#90% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
#par(mfrow=c(2,1))
ts.plot(magnification*spreadin3[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.3,0.2))
lines(mean.y.pred,col="red",lty=3,main="h=")
lines(low.y.pred,col="green",lty=3)
lines(up.y.pred,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

ts.plot(p_t,ylab="Predictive log-likelihod")
dev.off()

### GENERATING NEEDED DATA (as above i.e. df=5; h=1; T=180; CI=90%)
#2 in-sample
runin2uk=smc2_pred(N=N,T=T,y=magnification*spreadin2uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin2uk=runin2uk[[7]]
lowin2uk=runin2uk[[8]]
upin2uk=runin2uk[[9]]

#2 out-sample
runout2uk=smc2_pred(N=N,T=T,y=magnification*spreadout2uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout2uk=runout2uk[[7]]
lowout2uk=runout2uk[[8]]
upout2uk=runout2uk[[9]]

###

#8 in-sample
runin8uk=smc2_pred(N=N,T=T,y=magnification*spreadin8uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin8uk=runin8uk[[7]]
lowin8uk=runin8uk[[8]]
upin8uk=runin8uk[[9]]

#8 out-sample
runout8uk=smc2_pred(N=N,T=T,y=magnification*spreadout8uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout8uk=runout8uk[[7]]
lowout8uk=runout8uk[[8]]
upout8uk=runout8uk[[9]]

###

#12 in-sample
runin12uk=smc2_pred(N=N,T=T,y=magnification*spreadin12uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin12uk=runin12uk[[7]]
lowin12uk=runin12uk[[8]]
upin12uk=runin12uk[[9]]

#12 out-sample
runout12uk=smc2_pred(N=N,T=T,y=magnification*spreadout12uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout12uk=runout12uk[[7]]
lowout12uk=runout12uk[[8]]
upout12uk=runout12uk[[9]]

###

#16 in-sample
runin16uk=smc2_pred(N=N,T=T,y=magnification*spreadin16uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin16uk=runin16uk[[7]]
lowin16uk=runin16uk[[8]]
upin16uk=runin16uk[[9]]

#16 out-sample
runout16uk=smc2_pred(N=N,T=T,y=magnification*spreadout16uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout16uk=runout16uk[[7]]
lowout16uk=runout16uk[[8]]
upout16uk=runout16uk[[9]]

###

#20 in-sample
runin20uk=smc2_pred(N=N,T=T,y=magnification*spreadin20uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanin20uk=runin20uk[[7]]
lowin20uk=runin20uk[[8]]
upin20uk=runin20uk[[9]]

#20 out-sample
runout20uk=smc2_pred(N=N,T=T,y=magnification*spreadout20uk,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
meanout20uk=runout20uk[[7]]
lowout20uk=runout20uk[[8]]
upout20uk=runout20uk[[9]]
