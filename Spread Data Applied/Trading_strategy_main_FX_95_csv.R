setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("Trading_strategy_functions.R")
spreads_FX_95=read.csv("spreads_FX_95.csv",header=TRUE)
attach(spreads_FX_95)

T=125; h=1;

### MORE COMPACTLY
#Strategy 1
(out99=strategy(spread=NZDUSD_ZARUSD1999out,up.y.pred=upout99,low.y.pred=lowout99,mean.y.pred=meanout99,T=T,h=h))
(out00=strategy(spread=NZDUSD_ZARUSD2000out[20:128],up.y.pred=upout00[20:128],low.y.pred=lowout00[20:128],mean.y.pred=meanout00[20:128],T=108,h=h))
(out02=strategy(spread=NZDUSD_ZARUSD2002out,up.y.pred=upout02,low.y.pred=lowout02,mean.y.pred=meanout02,T=T,h=h))
(out07=strategy(spread=NZDUSD_ZARUSD2007out,up.y.pred=upout07,low.y.pred=lowout07,mean.y.pred=meanout07,T=T,h=h))
(out09=strategy(spread=NZDUSD_ZARUSD2009out,up.y.pred=upout09,low.y.pred=lowout09,mean.y.pred=meanout09,T=T,h=h))
(out12=strategy(spread=NZDUSD_ZARUSD2012out,up.y.pred=upout12,low.y.pred=lowout12,mean.y.pred=meanout12,T=T,h=h))

(out99_g=strategy(spread=NZDUSD_ZARUSD1999out,up.y.pred=upout99_g,low.y.pred=lowout99_g,mean.y.pred=meanout99_g,T=T,h=h))
(out00_g=strategy(spread=NZDUSD_ZARUSD2000out[20:128],up.y.pred=upout00_g[20:128],low.y.pred=lowout00_g[20:128],mean.y.pred=meanout00_g[20:128],T=108,h=h))
(out02_g=strategy(spread=NZDUSD_ZARUSD2002out,up.y.pred=upout02_g,low.y.pred=lowout02_g,mean.y.pred=meanout02_g,T=T,h=h))
(out07_g=strategy(spread=NZDUSD_ZARUSD2007out,up.y.pred=upout07_g,low.y.pred=lowout07_g,mean.y.pred=meanout07_g,T=T,h=h))
(out09_g=strategy(spread=NZDUSD_ZARUSD2009out,up.y.pred=upout09_g,low.y.pred=lowout09_g,mean.y.pred=meanout09_g,T=T,h=h))
(out12_g=strategy(spread=NZDUSD_ZARUSD2012out,up.y.pred=upout12_g,low.y.pred=lowout12_g,mean.y.pred=meanout12_g,T=T,h=h))

(out99_bb=strategy(spread=NZDUSD_ZARUSD1999out[20:length(NZDUSD_ZARUSD1999out)],up.y.pred=NZDUSD_ZARUSD1999outBB$up[20:length(NZDUSD_ZARUSD1999out)],low.y.pred=NZDUSD_ZARUSD1999outBB$dn[20:length(NZDUSD_ZARUSD1999out)],mean.y.pred=NZDUSD_ZARUSD1999outBB$mavg[20:length(NZDUSD_ZARUSD1999out)],T=112,h=0))
(out00_bb=strategy(spread=NZDUSD_ZARUSD2000out[20:length(NZDUSD_ZARUSD2000out)],up.y.pred=NZDUSD_ZARUSD2000outBB$up[20:length(NZDUSD_ZARUSD2000out)],low.y.pred=NZDUSD_ZARUSD2000outBB$dn[20:length(NZDUSD_ZARUSD2000out)],mean.y.pred=NZDUSD_ZARUSD2000outBB$mavg[20:length(NZDUSD_ZARUSD2000out)],T=109,h=0))
(out02_bb=strategy(spread=NZDUSD_ZARUSD2002out[20:length(NZDUSD_ZARUSD2002out)],up.y.pred=NZDUSD_ZARUSD2002outBB$up[20:length(NZDUSD_ZARUSD2002out)],low.y.pred=NZDUSD_ZARUSD2002outBB$dn[20:length(NZDUSD_ZARUSD2002out)],mean.y.pred=NZDUSD_ZARUSD2002outBB$mavg[20:length(NZDUSD_ZARUSD2002out)],T=109,h=0))
(out07_bb=strategy(spread=NZDUSD_ZARUSD2007out[20:length(NZDUSD_ZARUSD2007out)],up.y.pred=NZDUSD_ZARUSD2007outBB$up[20:length(NZDUSD_ZARUSD2007out)],low.y.pred=NZDUSD_ZARUSD2007outBB$dn[20:length(NZDUSD_ZARUSD2007out)],mean.y.pred=NZDUSD_ZARUSD2007outBB$mavg[20:length(NZDUSD_ZARUSD2007out)],T=113,h=0))
(out09_bb=strategy(spread=NZDUSD_ZARUSD2009out[20:length(NZDUSD_ZARUSD2009out)],up.y.pred=NZDUSD_ZARUSD2009outBB$up[20:length(NZDUSD_ZARUSD2009out)],low.y.pred=NZDUSD_ZARUSD2009outBB$dn[20:length(NZDUSD_ZARUSD2009out)],mean.y.pred=NZDUSD_ZARUSD2009outBB$mavg[20:length(NZDUSD_ZARUSD2009out)],T=113,h=0))
(out12_bb=strategy(spread=NZDUSD_ZARUSD2012out[20:length(NZDUSD_ZARUSD2012out)],up.y.pred=NZDUSD_ZARUSD2012outBB$up[20:length(NZDUSD_ZARUSD2012out)],low.y.pred=NZDUSD_ZARUSD2012outBB$dn[20:length(NZDUSD_ZARUSD2012out)],mean.y.pred=NZDUSD_ZARUSD2012outBB$mavg[20:length(NZDUSD_ZARUSD2012out)],T=112,h=0))

# Strategy 2
(out2.99=strategy2(spread=NZDUSD_ZARUSD1999out,up.y.pred=upout99,low.y.pred=lowout99,mean.y.pred=meanout99,T=T,h=h))
(out2.00=strategy2(spread=NZDUSD_ZARUSD2000out[20:128],up.y.pred=upout00[20:128],low.y.pred=lowout00[20:128],mean.y.pred=meanout00[20:128],T=108,h=h))
(out2.02=strategy2(spread=NZDUSD_ZARUSD2002out,up.y.pred=upout02,low.y.pred=lowout02,mean.y.pred=meanout02,T=T,h=h))
(out2.07=strategy2(spread=NZDUSD_ZARUSD2007out,up.y.pred=upout07,low.y.pred=lowout07,mean.y.pred=meanout07,T=T,h=h))
(out2.09=strategy2(spread=NZDUSD_ZARUSD2009out,up.y.pred=upout09,low.y.pred=lowout09,mean.y.pred=meanout09,T=T,h=h))
(out2.12=strategy2(spread=NZDUSD_ZARUSD2012out,up.y.pred=upout12,low.y.pred=lowout12,mean.y.pred=meanout12,T=T,h=h))

(out2.99_g=strategy2(spread=NZDUSD_ZARUSD1999out,up.y.pred=upout99_g,low.y.pred=lowout99_g,mean.y.pred=meanout99_g,T=T,h=h))
(out2.00_g=strategy2(spread=NZDUSD_ZARUSD2000out[20:128],up.y.pred=upout00_g[20:128],low.y.pred=lowout00_g[20:128],mean.y.pred=meanout00_g[20:128],T=108,h=h))
(out2.02_g=strategy2(spread=NZDUSD_ZARUSD2002out,up.y.pred=upout02_g,low.y.pred=lowout02_g,mean.y.pred=meanout02_g,T=T,h=h))
(out2.07_g=strategy2(spread=NZDUSD_ZARUSD2007out,up.y.pred=upout07_g,low.y.pred=lowout07_g,mean.y.pred=meanout07_g,T=T,h=h))
(out2.09_g=strategy2(spread=NZDUSD_ZARUSD2009out,up.y.pred=upout09_g,low.y.pred=lowout09_g,mean.y.pred=meanout09_g,T=T,h=h))
(out2.12_g=strategy2(spread=NZDUSD_ZARUSD2012out,up.y.pred=upout12_g,low.y.pred=lowout12_g,mean.y.pred=meanout12_g,T=T,h=h))

(out2.99_bb=strategy2(spread=NZDUSD_ZARUSD1999out[20:length(NZDUSD_ZARUSD1999out)],up.y.pred=NZDUSD_ZARUSD1999outBB$up[20:length(NZDUSD_ZARUSD1999out)],low.y.pred=NZDUSD_ZARUSD1999outBB$dn[20:length(NZDUSD_ZARUSD1999out)],mean.y.pred=NZDUSD_ZARUSD1999outBB$mavg[20:length(NZDUSD_ZARUSD1999out)],T=111,h=0))
(out2.00_bb=strategy2(spread=NZDUSD_ZARUSD2000out[20:length(NZDUSD_ZARUSD2000out)],up.y.pred=NZDUSD_ZARUSD2000outBB$up[20:length(NZDUSD_ZARUSD2000out)],low.y.pred=NZDUSD_ZARUSD2000outBB$dn[20:length(NZDUSD_ZARUSD2000out)],mean.y.pred=NZDUSD_ZARUSD2000outBB$mavg[20:length(NZDUSD_ZARUSD2000out)],T=109,h=0))
(out2.02_bb=strategy2(spread=NZDUSD_ZARUSD2002out[20:length(NZDUSD_ZARUSD2002out)],up.y.pred=NZDUSD_ZARUSD2002outBB$up[20:length(NZDUSD_ZARUSD2002out)],low.y.pred=NZDUSD_ZARUSD2002outBB$dn[20:length(NZDUSD_ZARUSD2002out)],mean.y.pred=NZDUSD_ZARUSD2002outBB$mavg[20:length(NZDUSD_ZARUSD2002out)],T=109,h=0))
(out2.07_bb=strategy2(spread=NZDUSD_ZARUSD2007out[20:length(NZDUSD_ZARUSD2007out)],up.y.pred=NZDUSD_ZARUSD2007outBB$up[20:length(NZDUSD_ZARUSD2007out)],low.y.pred=NZDUSD_ZARUSD2007outBB$dn[20:length(NZDUSD_ZARUSD2007out)],mean.y.pred=NZDUSD_ZARUSD2007outBB$mavg[20:length(NZDUSD_ZARUSD2007out)],T=112,h=0))
(out2.09_bb=strategy2(spread=NZDUSD_ZARUSD2009out[20:length(NZDUSD_ZARUSD2009out)],up.y.pred=NZDUSD_ZARUSD2009outBB$up[20:length(NZDUSD_ZARUSD2009out)],low.y.pred=NZDUSD_ZARUSD2009outBB$dn[20:length(NZDUSD_ZARUSD2009out)],mean.y.pred=NZDUSD_ZARUSD2009outBB$mavg[20:length(NZDUSD_ZARUSD2009out)],T=112,h=0))
(out2.12_bb=strategy2(spread=NZDUSD_ZARUSD2012out[20:length(NZDUSD_ZARUSD2012out)],up.y.pred=NZDUSD_ZARUSD2012outBB$up[20:length(NZDUSD_ZARUSD2012out)],low.y.pred=NZDUSD_ZARUSD2012outBB$dn[20:length(NZDUSD_ZARUSD2012out)],mean.y.pred=NZDUSD_ZARUSD2012outBB$mavg[20:length(NZDUSD_ZARUSD2012out)],T=112,h=0))

# Z-Score
os=2; cs=0.75; ol=-2; cl=-0.5

(outz.99=z.strategy(spread=NZDUSD_ZARUSD1999out,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.00=z.strategy(spread=NZDUSD_ZARUSD2000out,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.02=z.strategy(spread=NZDUSD_ZARUSD2002out,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.07=z.strategy(spread=NZDUSD_ZARUSD2007out,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.09=z.strategy(spread=NZDUSD_ZARUSD2009out,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.12=z.strategy(spread=NZDUSD_ZARUSD2012out,T=T,os=os,cs=cs,ol=ol,cl=cl))
