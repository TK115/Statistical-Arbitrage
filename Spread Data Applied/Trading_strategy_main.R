setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("Trading_strategy_functions.R")

#data located in "Spread_spx_main.R" for i=3,9,11,17,31,45 as follows for i=3:
# spreadin3; stockAin3; stockBin3
# spreadout3; stockAout3; stockBout3

#data also located in "Apply_data.R" as follows as follows for i=3:
#meanin3; lowin3; upin3
#meanout3; lowout3; upout3

#meanin3_g; lowin3_g; upin3_g
#meanout3_g; lowout3_g; upout3_g

### RESULTS tL-SSM
#95% credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin3[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin3,col="red",lty=3,main="h=")
lines(lowin3,col="green",lty=3)
lines(upin3,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in3=strategy(spread=magnification*spreadin3,up.y.pred=upin3,low.y.pred=lowin3,mean.y.pred=meanin3,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout3[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout3,col="red",lty=3,main="h=")
lines(lowout3,col="green",lty=3)
lines(upout3,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out3=strategy(spread=magnification*spreadout3,up.y.pred=upout3,low.y.pred=lowout3,mean.y.pred=meanout3,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin9[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanin9,col="red",lty=3,main="h=")
lines(lowin9,col="green",lty=3)
lines(upin9,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in9=strategy(spread=magnification*spreadin9,up.y.pred=upin9,low.y.pred=lowin9,mean.y.pred=meanin9,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout9[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanout9,col="red",lty=3,main="h=")
lines(lowout9,col="green",lty=3)
lines(upout9,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out9=strategy(spread=magnification*spreadout9,up.y.pred=upout9,low.y.pred=lowout9,mean.y.pred=meanout9,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin11[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin11,col="red",lty=3,main="h=")
lines(lowin11,col="green",lty=3)
lines(upin11,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in11=strategy(spread=magnification*spreadin11,up.y.pred=upin11,low.y.pred=lowin11,mean.y.pred=meanin11,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout11[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout11,col="red",lty=3,main="h=")
lines(lowout11,col="green",lty=3)
lines(upout11,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out11=strategy(spread=magnification*spreadout11,up.y.pred=upout11,low.y.pred=lowout11,mean.y.pred=meanout11,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin17[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin17,col="red",lty=3,main="h=")
lines(lowin17,col="green",lty=3)
lines(upin17,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in17=strategy(spread=magnification*spreadin17,up.y.pred=upin17,low.y.pred=lowin17,mean.y.pred=meanin17,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout17[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout17,col="red",lty=3,main="h=")
lines(lowout17,col="green",lty=3)
lines(upout17,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out17=strategy(spread=magnification*spreadout17,up.y.pred=upout17,low.y.pred=lowout17,mean.y.pred=meanout17,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin31[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanin31,col="red",lty=3,main="h=")
lines(lowin31,col="green",lty=3)
lines(upin31,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in31=strategy(spread=magnification*spreadin31,up.y.pred=upin31,low.y.pred=lowin31,mean.y.pred=meanin31,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout31[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.14,0.04))
lines(meanout31,col="red",lty=3,main="h=")
lines(lowout31,col="green",lty=3)
lines(upout31,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out31=strategy(spread=magnification*spreadout31,up.y.pred=upout31,low.y.pred=lowout31,mean.y.pred=meanout31,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin45[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin45,col="red",lty=3,main="h=")
lines(lowin45,col="green",lty=3)
lines(upin45,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in45=strategy(spread=magnification*spreadin45,up.y.pred=upin45,low.y.pred=lowin45,mean.y.pred=meanin45,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout45[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout45,col="red",lty=3,main="h=")
lines(lowout45,col="green",lty=3)
lines(upout45,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out45=strategy(spread=magnification*spreadout45,up.y.pred=upout45,low.y.pred=lowout45,mean.y.pred=meanout45,T=T,h=h))




### RESULTS GL-SSM
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin3[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin3_g,col="red",lty=3,main="h=")
lines(lowin3_g,col="green",lty=3)
lines(upin3_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in3_g=strategy(spread=magnification*spreadin3,up.y.pred=upin3_g,low.y.pred=lowin3_g,mean.y.pred=meanin3_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout3[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout3_g,col="red",lty=3,main="h=")
lines(lowout3_g,col="green",lty=3)
lines(upout3_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out3_g=strategy(spread=magnification*spreadout3,up.y.pred=upout3_g,low.y.pred=lowout3_g,mean.y.pred=meanout3_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin9[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin9_g,col="red",lty=3,main="h=")
lines(lowin9_g,col="green",lty=3)
lines(upin9_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in9_g=strategy(spread=magnification*spreadin9,up.y.pred=upin9_g,low.y.pred=lowin9_g,mean.y.pred=meanin9_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout9[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.05,0.05))
lines(meanout9_g,col="red",lty=3,main="h=")
lines(lowout9_g,col="green",lty=3)
lines(upout9_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out9_g=strategy(spread=magnification*spreadout9,up.y.pred=upout9_g,low.y.pred=lowout9_g,mean.y.pred=meanout9_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin11[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin11_g,col="red",lty=3,main="h=")
lines(lowin11_g,col="green",lty=3)
lines(upin11_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in11_g=strategy(spread=magnification*spreadin11,up.y.pred=upin11_g,low.y.pred=lowin11_g,mean.y.pred=meanin11_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout11[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout11_g,col="red",lty=3,main="h=")
lines(lowout11_g,col="green",lty=3)
lines(upout11_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out11_g=strategy(spread=magnification*spreadout11,up.y.pred=upout11_g,low.y.pred=lowout11_g,mean.y.pred=meanout11_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin17[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin17_g,col="red",lty=3,main="h=")
lines(lowin17_g,col="green",lty=3)
lines(upin17_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in17_g=strategy(spread=magnification*spreadin17,up.y.pred=upin17_g,low.y.pred=lowin17_g,mean.y.pred=meanin17_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout17[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout17_g,col="red",lty=3,main="h=")
lines(lowout17_g,col="green",lty=3)
lines(upout17_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out17_g=strategy(spread=magnification*spreadout17,up.y.pred=upout17_g,low.y.pred=lowout17_g,mean.y.pred=meanout17_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin31[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.04,0.04))
lines(meanin31_g,col="red",lty=3,main="h=")
lines(lowin31_g,col="green",lty=3)
lines(upin31_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in31_g=strategy(spread=magnification*spreadin31,up.y.pred=upin31_g,low.y.pred=lowin31_g,mean.y.pred=meanin31_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout31[(h+1):T],ylab="",main=paste("t Model with h=", h),ylim=c(-0.14,0.04))
lines(meanout31_g,col="red",lty=3,main="h=")
lines(lowout31_g,col="green",lty=3)
lines(upout31_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out31_g=strategy(spread=magnification*spreadout31,up.y.pred=upout31_g,low.y.pred=lowout31_g,mean.y.pred=meanout31_g,T=T,h=h))

#
#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadin45[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanin45_g,col="red",lty=3,main="h=")
lines(lowin45_g,col="green",lty=3)
lines(upin45_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(in45_g=strategy(spread=magnification*spreadin45,up.y.pred=upin45_g,low.y.pred=lowin45_g,mean.y.pred=meanin45_g,T=T,h=h))

###

#credible prediction interval of (y_t+h|y_0:t); shift y by h in plot
ts.plot(magnification*spreadout45[(h+1):T],ylab="",main=paste("t Model with h=", h))
lines(meanout45_g,col="red",lty=3,main="h=")
lines(lowout45_g,col="green",lty=3)
lines(upout45_g,col="green",lty=3)
legend("topleft",c("y_t+h","E(y_t+h|y_0:t)",paste((ci.u-ci.l)*100,"% CI of (y_t+h|y_0:t)")),lty=c(1,3,3,3),col=c("black","red","green","green"),cex=0.5)

(out45_g=strategy(spread=magnification*spreadout45,up.y.pred=upout45_g,low.y.pred=lowout45_g,mean.y.pred=meanout45_g,T=T,h=h))
