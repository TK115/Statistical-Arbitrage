setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("Trading_strategy_functions.R")
spreads_20_10_95=read.csv("spreads_20_10_95.csv",header=TRUE)
attach(spreads_20_10_95)

T=length(sp3out)-1; h=1;

### MORE COMPACTLY
#Strategy 1
(out3=strategy(spread=sp3out,up.y.pred=upout3,low.y.pred=lowout3,mean.y.pred=meanout3,T=T,h=h))
(out9=strategy(spread=sp9out,up.y.pred=upout9,low.y.pred=lowout9,mean.y.pred=meanout9,T=T,h=h))
(out11=strategy(spread=sp11out,up.y.pred=upout11,low.y.pred=lowout11,mean.y.pred=meanout11,T=T,h=h))
(out17=strategy(spread=sp17out,up.y.pred=upout17,low.y.pred=lowout17,mean.y.pred=meanout17,T=T,h=h))
(out31=strategy(spread=sp31out,up.y.pred=upout31,low.y.pred=lowout31,mean.y.pred=meanout31,T=T,h=h))
(out45=strategy(spread=sp45out,up.y.pred=upout45,low.y.pred=lowout45,mean.y.pred=meanout45,T=T,h=h))

(out3_g=strategy(spread=sp3out,up.y.pred=upout3_g,low.y.pred=lowout3_g,mean.y.pred=meanout3_g,T=T,h=h))
(out9_g=strategy(spread=sp9out,up.y.pred=upout9_g,low.y.pred=lowout9_g,mean.y.pred=meanout9_g,T=T,h=h))
(out11_g=strategy(spread=sp11out,up.y.pred=upout11_g,low.y.pred=lowout11_g,mean.y.pred=meanout11_g,T=T,h=h))
(out17_g=strategy(spread=sp17out,up.y.pred=upout17_g,low.y.pred=lowout17_g,mean.y.pred=meanout17_g,T=T,h=h))
(out31_g=strategy(spread=sp31out,up.y.pred=upout31_g,low.y.pred=lowout31_g,mean.y.pred=meanout31_g,T=T,h=h))
(out45_g=strategy(spread=sp45out,up.y.pred=upout45_g,low.y.pred=lowout45_g,mean.y.pred=meanout45_g,T=T,h=h))

(out3_bb=strategy(spread=spreadout3[20:length(spreadout3)],up.y.pred=spreadout3BB$up[20:length(spreadout3)],low.y.pred=spreadout3BB$dn[20:length(spreadout3)],mean.y.pred=spreadout3BB$mavg[20:length(spreadout3)],T=161,h=0))
(out9_bb=strategy(spread=spreadout9[20:length(spreadout9)],up.y.pred=spreadout9BB$up[20:length(spreadout9)],low.y.pred=spreadout9BB$dn[20:length(spreadout9)],mean.y.pred=spreadout9BB$mavg[20:length(spreadout9)],T=161,h=0))
(out11_bb=strategy(spread=spreadout11,up.y.pred=spreadout11BB$up[20:length(spreadout11)],low.y.pred=spreadout11BB$dn[20:length(spreadout11)],mean.y.pred=spreadout11BB$mavg[20:length(spreadout11)],T=161,h=0))
(out17_bb=strategy(spread=spreadout17,up.y.pred=spreadout17BB$up[20:length(spreadout17)],low.y.pred=spreadout17BB$dn[20:length(spreadout17)],mean.y.pred=spreadout17BB$mavg[20:length(spreadout17)],T=161,h=0))
(out31_bb=strategy(spread=spreadout31,up.y.pred=spreadout31BB$up[20:length(spreadout31)],low.y.pred=spreadout31BB$dn[20:length(spreadout31)],mean.y.pred=spreadout31BB$mavg[20:length(spreadout31)],T=161,h=0))
(out45_bb=strategy(spread=spreadout45,up.y.pred=spreadout45BB$up[20:length(spreadout45)],low.y.pred=spreadout45BB$dn[20:length(spreadout45)],mean.y.pred=spreadout45BB$mavg[20:length(spreadout45)],T=161,h=0))

# Strategy 2
(out2.3=strategy2(spread=sp3out,up.y.pred=upout3,low.y.pred=lowout3,mean.y.pred=meanout3,T=T,h=h))
(out2.9=strategy2(spread=sp9out,up.y.pred=upout9,low.y.pred=lowout9,mean.y.pred=meanout9,T=T,h=h))
(out2.11=strategy2(spread=sp11out,up.y.pred=upout11,low.y.pred=lowout11,mean.y.pred=meanout11,T=T,h=h))
(out2.17=strategy2(spread=sp17out,up.y.pred=upout17,low.y.pred=lowout17,mean.y.pred=meanout17,T=T,h=h))
(out2.31=strategy2(spread=sp31out,up.y.pred=upout31,low.y.pred=lowout31,mean.y.pred=meanout31,T=T,h=h))
(out2.45=strategy2(spread=sp45out,up.y.pred=upout45,low.y.pred=lowout45,mean.y.pred=meanout45,T=T,h=h))

(out2.3_g=strategy2(spread=sp3out,up.y.pred=upout3_g,low.y.pred=lowout3_g,mean.y.pred=meanout3_g,T=T,h=h))
(out2.9_g=strategy2(spread=sp9out,up.y.pred=upout9_g,low.y.pred=lowout9_g,mean.y.pred=meanout9_g,T=T,h=h))
(out2.11_g=strategy2(spread=sp11out,up.y.pred=upout11_g,low.y.pred=lowout11_g,mean.y.pred=meanout11_g,T=T,h=h))
(out2.17_g=strategy2(spread=sp17out,up.y.pred=upout17_g,low.y.pred=lowout17_g,mean.y.pred=meanout17_g,T=T,h=h))
(out2.31_g=strategy2(spread=sp31out,up.y.pred=upout31_g,low.y.pred=lowout31_g,mean.y.pred=meanout31_g,T=T,h=h))
(out2.45_g=strategy2(spread=sp45out,up.y.pred=upout45_g,low.y.pred=lowout45_g,mean.y.pred=meanout45_g,T=T,h=h))

(out2.3_bb=strategy2(spread=spreadout3[20:length(spreadout3)],up.y.pred=spreadout3BB$up[20:length(spreadout3)],low.y.pred=spreadout3BB$dn[20:length(spreadout3)],mean.y.pred=spreadout3BB$mavg[20:length(spreadout3)],T=161,h=0))
(out2.9_bb=strategy2(spread=spreadout9[20:length(spreadout9)],up.y.pred=spreadout9BB$up[20:length(spreadout9)],low.y.pred=spreadout9BB$dn[20:length(spreadout9)],mean.y.pred=spreadout9BB$mavg[20:length(spreadout9)],T=161,h=0))
(out2.11_bb=strategy2(spread=spreadout11,up.y.pred=spreadout11BB$up[20:length(spreadout11)],low.y.pred=spreadout11BB$dn[20:length(spreadout11)],mean.y.pred=spreadout11BB$mavg[20:length(spreadout11)],T=161,h=0))
(out2.17_bb=strategy2(spread=spreadout17,up.y.pred=spreadout17BB$up[20:length(spreadout17)],low.y.pred=spreadout17BB$dn[20:length(spreadout17)],mean.y.pred=spreadout17BB$mavg[20:length(spreadout17)],T=161,h=0))
(out2.31_bb=strategy2(spread=spreadout31,up.y.pred=spreadout31BB$up[20:length(spreadout31)],low.y.pred=spreadout31BB$dn[20:length(spreadout31)],mean.y.pred=spreadout31BB$mavg[20:length(spreadout31)],T=161,h=0))
(out2.45_bb=strategy2(spread=spreadout45,up.y.pred=spreadout45BB$up[20:length(spreadout45)],low.y.pred=spreadout45BB$dn[20:length(spreadout45)],mean.y.pred=spreadout45BB$mavg[20:length(spreadout45)],T=161,h=0))

# Z-Score
os=2; cs=0.75; ol=-2; cl=-0.5

(outz.3=z.strategy(spread=spreadout3,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.9=z.strategy(spread=spreadout9,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.11=z.strategy(spread=spreadout11,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.17=z.strategy(spread=spreadout17,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.31=z.strategy(spread=spreadout31,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.45=z.strategy(spread=spreadout45,T=T,os=os,cs=cs,ol=ol,cl=cl))
