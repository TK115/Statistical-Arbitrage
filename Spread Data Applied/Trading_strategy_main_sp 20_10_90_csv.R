setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("Trading_strategy_functions.R")
spreads_20_10_90=read.csv("spreads_20_10_90.csv",header=TRUE)
attach(spreads_20_10_90)

# 20 stocks; spread factor of 10; 90% CI

T=length(sp3in)-1; h=1; ci.u=0.95; ci.l=0.05

#data located in "Spread_spx_main.R" for i=3,9,11,17,31,45 as follows for i=3:
# spreadin3; stockAin3; stockBin3
# spreadout3; stockAout3; stockBout3

#data also located in "Apply_data.R" as follows as follows for i=3:
#meanin3; lowin3; upin3
#meanout3; lowout3; upout3

#meanin3_g; lowin3_g; upin3_g
#meanout3_g; lowout3_g; upout3_g

### RESULTS tL-SSM
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp3in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin3,col="red",lty=3,main="h=")
lines(lowin3,col="green",lty=3)
lines(upin3,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in3=strategy(spread=sp3in,up.y.pred=upin3,low.y.pred=lowin3,mean.y.pred=meanin3,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp3out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout3,col="red",lty=3,main="h=")
lines(lowout3,col="green",lty=3)
lines(upout3,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out3=strategy(spread=sp3out,up.y.pred=upout3,low.y.pred=lowout3,mean.y.pred=meanout3,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp9in[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanin9,col="red",lty=3,main="h=")
lines(lowin9,col="green",lty=3)
lines(upin9,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in9=strategy(spread=sp9in,up.y.pred=upin9,low.y.pred=lowin9,mean.y.pred=meanin9,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp9out[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanout9,col="red",lty=3,main="h=")
lines(lowout9,col="green",lty=3)
lines(upout9,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out9=strategy(spread=sp9out,up.y.pred=upout9,low.y.pred=lowout9,mean.y.pred=meanout9,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp11in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin11,col="red",lty=3,main="h=")
lines(lowin11,col="green",lty=3)
lines(upin11,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in11=strategy(spread=sp11in,up.y.pred=upin11,low.y.pred=lowin11,mean.y.pred=meanin11,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp11out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout11,col="red",lty=3,main="h=")
lines(lowout11,col="green",lty=3)
lines(upout11,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out11=strategy(spread=sp11out,up.y.pred=upout11,low.y.pred=lowout11,mean.y.pred=meanout11,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp17in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin17,col="red",lty=3,main="h=")
lines(lowin17,col="green",lty=3)
lines(upin17,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in17=strategy(spread=sp17in,up.y.pred=upin17,low.y.pred=lowin17,mean.y.pred=meanin17,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp17out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout17,col="red",lty=3,main="h=")
lines(lowout17,col="green",lty=3)
lines(upout17,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out17=strategy(spread=sp17out,up.y.pred=upout17,low.y.pred=lowout17,mean.y.pred=meanout17,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp31in[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanin31,col="red",lty=3,main="h=")
lines(lowin31,col="green",lty=3)
lines(upin31,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in31=strategy(spread=sp31in,up.y.pred=upin31,low.y.pred=lowin31,mean.y.pred=meanin31,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp31out[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.14,0.04))
lines(meanout31,col="red",lty=3,main="h=")
lines(lowout31,col="green",lty=3)
lines(upout31,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out31=strategy(spread=sp31out,up.y.pred=upout31,low.y.pred=lowout31,mean.y.pred=meanout31,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp45in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin45,col="red",lty=3,main="h=")
lines(lowin45,col="green",lty=3)
lines(upin45,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in45=strategy(spread=sp45in,up.y.pred=upin45,low.y.pred=lowin45,mean.y.pred=meanin45,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp45out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout45,col="red",lty=3,main="h=")
lines(lowout45,col="green",lty=3)
lines(upout45,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out45=strategy(spread=sp45out,up.y.pred=upout45,low.y.pred=lowout45,mean.y.pred=meanout45,T=T,h=h))




### RESULTS GL-SSM
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp3in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin3_g,col="red",lty=3,main="h=")
lines(lowin3_g,col="green",lty=3)
lines(upin3_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in3_g=strategy(spread=sp3in,up.y.pred=upin3_g,low.y.pred=lowin3_g,mean.y.pred=meanin3_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp3out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout3_g,col="red",lty=3,main="h=")
lines(lowout3_g,col="green",lty=3)
lines(upout3_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out3_g=strategy(spread=sp3out,up.y.pred=upout3_g,low.y.pred=lowout3_g,mean.y.pred=meanout3_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp9in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin9_g,col="red",lty=3,main="h=")
lines(lowin9_g,col="green",lty=3)
lines(upin9_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in9_g=strategy(spread=sp9in,up.y.pred=upin9_g,low.y.pred=lowin9_g,mean.y.pred=meanin9_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp9out[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanout9_g,col="red",lty=3,main="h=")
lines(lowout9_g,col="green",lty=3)
lines(upout9_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out9_g=strategy(spread=sp9out,up.y.pred=upout9_g,low.y.pred=lowout9_g,mean.y.pred=meanout9_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp11in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin11_g,col="red",lty=3,main="h=")
lines(lowin11_g,col="green",lty=3)
lines(upin11_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in11_g=strategy(spread=sp11in,up.y.pred=upin11_g,low.y.pred=lowin11_g,mean.y.pred=meanin11_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp11out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout11_g,col="red",lty=3,main="h=")
lines(lowout11_g,col="green",lty=3)
lines(upout11_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out11_g=strategy(spread=sp11out,up.y.pred=upout11_g,low.y.pred=lowout11_g,mean.y.pred=meanout11_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp17in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin17_g,col="red",lty=3,main="h=")
lines(lowin17_g,col="green",lty=3)
lines(upin17_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in17_g=strategy(spread=sp17in,up.y.pred=upin17_g,low.y.pred=lowin17_g,mean.y.pred=meanin17_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp17out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout17_g,col="red",lty=3,main="h=")
lines(lowout17_g,col="green",lty=3)
lines(upout17_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out17_g=strategy(spread=sp17out,up.y.pred=upout17_g,low.y.pred=lowout17_g,mean.y.pred=meanout17_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp31in[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.04,0.04))
lines(meanin31_g,col="red",lty=3,main="h=")
lines(lowin31_g,col="green",lty=3)
lines(upin31_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in31_g=strategy(spread=sp31in,up.y.pred=upin31_g,low.y.pred=lowin31_g,mean.y.pred=meanin31_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp31out[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.14,0.04))
lines(meanout31_g,col="red",lty=3,main="h=")
lines(lowout31_g,col="green",lty=3)
lines(upout31_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out31_g=strategy(spread=sp31out,up.y.pred=upout31_g,low.y.pred=lowout31_g,mean.y.pred=meanout31_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp45in[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin45_g,col="red",lty=3,main="h=")
lines(lowin45_g,col="green",lty=3)
lines(upin45_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in45_g=strategy(spread=sp45in,up.y.pred=upin45_g,low.y.pred=lowin45_g,mean.y.pred=meanin45_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(sp45out[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout45_g,col="red",lty=3,main="h=")
lines(lowout45_g,col="green",lty=3)
lines(upout45_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out45_g=strategy(spread=sp45out,up.y.pred=upout45_g,low.y.pred=lowout45_g,mean.y.pred=meanout45_g,T=T,h=h))

### MORE COMPACTLY
#Strategy 1
#(in3=strategy(spread=sp3in,up.y.pred=upin3,low.y.pred=lowin3,mean.y.pred=meanin3,T=T,h=h))
(out3=strategy(spread=sp3out,up.y.pred=upout3,low.y.pred=lowout3,mean.y.pred=meanout3,T=T,h=h))
#(in9=strategy(spread=sp9in,up.y.pred=upin9,low.y.pred=lowin9,mean.y.pred=meanin9,T=T,h=h))
(out9=strategy(spread=sp9out,up.y.pred=upout9,low.y.pred=lowout9,mean.y.pred=meanout9,T=T,h=h))
#(in11=strategy(spread=sp11in,up.y.pred=upin11,low.y.pred=lowin11,mean.y.pred=meanin11,T=T,h=h))
(out11=strategy(spread=sp11out,up.y.pred=upout11,low.y.pred=lowout11,mean.y.pred=meanout11,T=T,h=h))
#(in17=strategy(spread=sp17in,up.y.pred=upin17,low.y.pred=lowin17,mean.y.pred=meanin17,T=T,h=h))
(out17=strategy(spread=sp17out,up.y.pred=upout17,low.y.pred=lowout17,mean.y.pred=meanout17,T=T,h=h))
#(in31=strategy(spread=sp31in,up.y.pred=upin31,low.y.pred=lowin31,mean.y.pred=meanin31,T=T,h=h))
(out31=strategy(spread=sp31out,up.y.pred=upout31,low.y.pred=lowout31,mean.y.pred=meanout31,T=T,h=h))
#(in45=strategy(spread=sp45in,up.y.pred=upin45,low.y.pred=lowin45,mean.y.pred=meanin45,T=T,h=h))
(out45=strategy(spread=sp45out,up.y.pred=upout45,low.y.pred=lowout45,mean.y.pred=meanout45,T=T,h=h))

#(in3_g=strategy(spread=sp3in,up.y.pred=upin3_g,low.y.pred=lowin3_g,mean.y.pred=meanin3_g,T=T,h=h))
(out3_g=strategy(spread=sp3out,up.y.pred=upout3_g,low.y.pred=lowout3_g,mean.y.pred=meanout3_g,T=T,h=h))
#(in9_g=strategy(spread=sp9in,up.y.pred=upin9_g,low.y.pred=lowin9_g,mean.y.pred=meanin9_g,T=T,h=h))
(out9_g=strategy(spread=sp9out,up.y.pred=upout9_g,low.y.pred=lowout9_g,mean.y.pred=meanout9_g,T=T,h=h))
#(in11_g=strategy(spread=sp11in,up.y.pred=upin11_g,low.y.pred=lowin11_g,mean.y.pred=meanin11_g,T=T,h=h))
(out11_g=strategy(spread=sp11out,up.y.pred=upout11_g,low.y.pred=lowout11_g,mean.y.pred=meanout11_g,T=T,h=h))
#(in17_g=strategy(spread=sp17in,up.y.pred=upin17_g,low.y.pred=lowin17_g,mean.y.pred=meanin17_g,T=T,h=h))
(out17_g=strategy(spread=sp17out,up.y.pred=upout17_g,low.y.pred=lowout17_g,mean.y.pred=meanout17_g,T=T,h=h))
#(in31_g=strategy(spread=sp31in,up.y.pred=upin31_g,low.y.pred=lowin31_g,mean.y.pred=meanin31_g,T=T,h=h))
(out31_g=strategy(spread=sp31out,up.y.pred=upout31_g,low.y.pred=lowout31_g,mean.y.pred=meanout31_g,T=T,h=h))
#(in45_g=strategy(spread=sp45in,up.y.pred=upin45_g,low.y.pred=lowin45_g,mean.y.pred=meanin45_g,T=T,h=h))
(out45_g=strategy(spread=sp45out,up.y.pred=upout45_g,low.y.pred=lowout45_g,mean.y.pred=meanout45_g,T=T,h=h))

# Strategy 2
#(in2.3=strategy2(spread=sp3in,up.y.pred=upin3,low.y.pred=lowin3,mean.y.pred=meanin3,T=T,h=h))
(out2.3=strategy2(spread=sp3out,up.y.pred=upout3,low.y.pred=lowout3,mean.y.pred=meanout3,T=T,h=h))
#(in2.9=strategy2(spread=sp9in,up.y.pred=upin9,low.y.pred=lowin9,mean.y.pred=meanin9,T=T,h=h))
(out2.9=strategy2(spread=sp9out,up.y.pred=upout9,low.y.pred=lowout9,mean.y.pred=meanout9,T=T,h=h))
#(in2.11=strategy2(spread=sp11in,up.y.pred=upin11,low.y.pred=lowin11,mean.y.pred=meanin11,T=T,h=h))
(out2.11=strategy2(spread=sp11out,up.y.pred=upout11,low.y.pred=lowout11,mean.y.pred=meanout11,T=T,h=h))
#(in2.17=strategy2(spread=sp17in,up.y.pred=upin17,low.y.pred=lowin17,mean.y.pred=meanin17,T=T,h=h))
(out2.17=strategy2(spread=sp17out,up.y.pred=upout17,low.y.pred=lowout17,mean.y.pred=meanout17,T=T,h=h))
#(in2.31=strategy2(spread=sp31in,up.y.pred=upin31,low.y.pred=lowin31,mean.y.pred=meanin31,T=T,h=h))
(out2.31=strategy2(spread=sp31out,up.y.pred=upout31,low.y.pred=lowout31,mean.y.pred=meanout31,T=T,h=h))
#(in2.45=strategy2(spread=sp45in,up.y.pred=upin45,low.y.pred=lowin45,mean.y.pred=meanin45,T=T,h=h))
(out2.45=strategy2(spread=sp45out,up.y.pred=upout45,low.y.pred=lowout45,mean.y.pred=meanout45,T=T,h=h))

#(in2.3_g=strategy2(spread=sp3in,up.y.pred=upin3_g,low.y.pred=lowin3_g,mean.y.pred=meanin3_g,T=T,h=h))
(out2.3_g=strategy2(spread=sp3out,up.y.pred=upout3_g,low.y.pred=lowout3_g,mean.y.pred=meanout3_g,T=T,h=h))
#(in2.9_g=strategy2(spread=sp9in,up.y.pred=upin9_g,low.y.pred=lowin9_g,mean.y.pred=meanin9_g,T=T,h=h))
(out2.9_g=strategy2(spread=sp9out,up.y.pred=upout9_g,low.y.pred=lowout9_g,mean.y.pred=meanout9_g,T=T,h=h))
#(in2.11_g=strategy2(spread=sp11in,up.y.pred=upin11_g,low.y.pred=lowin11_g,mean.y.pred=meanin11_g,T=T,h=h))
(out2.11_g=strategy2(spread=sp11out,up.y.pred=upout11_g,low.y.pred=lowout11_g,mean.y.pred=meanout11_g,T=T,h=h))
#(in2.17_g=strategy2(spread=sp17in,up.y.pred=upin17_g,low.y.pred=lowin17_g,mean.y.pred=meanin17_g,T=T,h=h))
(out2.17_g=strategy2(spread=sp17out,up.y.pred=upout17_g,low.y.pred=lowout17_g,mean.y.pred=meanout17_g,T=T,h=h))
#(in2.31_g=strategy2(spread=sp31in,up.y.pred=upin31_g,low.y.pred=lowin31_g,mean.y.pred=meanin31_g,T=T,h=h))
(out2.31_g=strategy2(spread=sp31out,up.y.pred=upout31_g,low.y.pred=lowout31_g,mean.y.pred=meanout31_g,T=T,h=h))
#(in2.45_g=strategy2(spread=sp45in,up.y.pred=upin45_g,low.y.pred=lowin45_g,mean.y.pred=meanin45_g,T=T,h=h))
(out2.45_g=strategy2(spread=sp45out,up.y.pred=upout45_g,low.y.pred=lowout45_g,mean.y.pred=meanout45_g,T=T,h=h))

# Z-Score
os=1.65; cs=0.75; ol=-1.65; cl=-0.75

#(inz.3=z.strategy(spread=sp3in,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.3=z.strategy(spread=sp3out,T=T,os=os,cs=cs,ol=ol,cl=cl))
#(inz.9=z.strategy(spread=sp9in,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.9=z.strategy(spread=sp9out,T=T,os=os,cs=cs,ol=ol,cl=cl))
#(inz.11=z.strategy(spread=sp11in,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.11=z.strategy(spread=sp11out,T=T,os=os,cs=cs,ol=ol,cl=cl))
#(inz.17=z.strategy(spread=sp17in,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.17=z.strategy(spread=sp17out,T=T,os=os,cs=cs,ol=ol,cl=cl))
#(inz.31=z.strategy(spread=sp31in,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.31=z.strategy(spread=sp31out,T=T,os=os,cs=cs,ol=ol,cl=cl))
#(inz.45=z.strategy(spread=sp45in,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.45=z.strategy(spread=sp45out,T=T,os=os,cs=cs,ol=ol,cl=cl))
