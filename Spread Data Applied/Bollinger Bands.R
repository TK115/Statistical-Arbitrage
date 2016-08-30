library(TTR)

# plot Bollinger bands
plot(spreadout3BB$spreadout3,type="l",xlab="Time (in days)",ylab="Logarithmic Price (USD)",main="SPX 1991 Spread and Bollinger Bands",ylim=c(-0.03,0.005))
lines(spreadout3BB$mavg,col="green")
lines(spreadout3BB$up,col="red")
lines(spreadout3BB$dn,col="red")
legend("bottomleft",c("Spread","EMA","Bollinger Bands"),lty=c(1,1,1),col=c("black","green","red"),cex=1)

# SPX
bbspreadout3=BBands(spreadout3,n=20,maType=EMA,sd=2)
spreadout3BB = data.frame(spreadout3,bbspreadout3)

bbspreadout9=BBands(spreadout9,n=20,maType=EMA,sd=2)
spreadout9BB = data.frame(spreadout9,bbspreadout9)

bbspreadout11=BBands(spreadout11,n=20,maType=EMA,sd=2)
spreadout11BB = data.frame(spreadout11,bbspreadout11)

bbspreadout17=BBands(spreadout17,n=20,maType=EMA,sd=2)
spreadout17BB = data.frame(spreadout17,bbspreadout17)

bbspreadout31=BBands(spreadout31,n=20,maType=EMA,sd=2)
spreadout31BB = data.frame(spreadout31,bbspreadout31)

bbspreadout45=BBands(spreadout45,n=20,maType=EMA,sd=2)
spreadout45BB = data.frame(spreadout45,bbspreadout45)

# UKX
bbspreadout2uk=BBands(spreadout2uk,n=20,maType=EMA,sd=2)
spreadout2ukBB = data.frame(spreadout2uk,bbspreadout2uk)

bbspreadout8uk=BBands(spreadout8uk,n=20,maType=EMA,sd=2)
spreadout8ukBB = data.frame(spreadout8uk,bbspreadout8uk)

bbspreadout12uk=BBands(spreadout12uk,n=20,maType=EMA,sd=2)
spreadout12ukBB = data.frame(spreadout12uk,bbspreadout12uk)

bbspreadout16uk=BBands(spreadout16uk,n=20,maType=EMA,sd=2)
spreadout16ukBB = data.frame(spreadout16uk,bbspreadout17)

bbspreadout20uk=BBands(spreadout20uk,n=20,maType=EMA,sd=2)
spreadout20ukBB = data.frame(spreadout20uk,bbspreadout20uk)

# FX
bbNZDUSD_ZARUSD1999out=BBands(NZDUSD_ZARUSD1999out,n=20,maType=EMA,sd=2)
NZDUSD_ZARUSD1999outBB = data.frame(NZDUSD_ZARUSD1999out,bbNZDUSD_ZARUSD1999out)

bbNZDUSD_ZARUSD2000out=BBands(NZDUSD_ZARUSD2000out,n=20,maType=EMA,sd=2)
NZDUSD_ZARUSD2000outBB = data.frame(NZDUSD_ZARUSD2000out,bbNZDUSD_ZARUSD2000out)

bbNZDUSD_ZARUSD2002out=BBands(NZDUSD_ZARUSD2002out,n=20,maType=EMA,sd=2)
NZDUSD_ZARUSD2002outBB = data.frame(NZDUSD_ZARUSD2002out,bbNZDUSD_ZARUSD2002out)

bbNZDUSD_ZARUSD2007out=BBands(NZDUSD_ZARUSD2007out,n=20,maType=EMA,sd=2)
NZDUSD_ZARUSD2007outBB = data.frame(NZDUSD_ZARUSD2007out,bbNZDUSD_ZARUSD2007out)

bbNZDUSD_ZARUSD2009out=BBands(NZDUSD_ZARUSD2009out,n=20,maType=EMA,sd=2)
NZDUSD_ZARUSD2009outBB = data.frame(NZDUSD_ZARUSD2009out,bbNZDUSD_ZARUSD2009out)

bbNZDUSD_ZARUSD2012out=BBands(NZDUSD_ZARUSD2012out,n=20,maType=EMA,sd=2)
NZDUSD_ZARUSD2012outBB = data.frame(NZDUSD_ZARUSD2012out,bbNZDUSD_ZARUSD2012out)
