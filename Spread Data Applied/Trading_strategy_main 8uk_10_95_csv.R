setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread Data Applied")
source("Trading_strategy_functions.R")
spreads_8uk_10_95=read.csv("spreads_8uk_10_95.csv",header=TRUE)
attach(spreads_8uk_10_95)

T=length(sp2ukout)-1; h=1;

### MORE COMPACTLY
#Strategy 1
(out2uk=strategy(spread=sp2ukout,up.y.pred=upout2uk,low.y.pred=lowout2uk,mean.y.pred=meanout2uk,T=T,h=h))
(out8uk=strategy(spread=sp8ukout,up.y.pred=upout8uk,low.y.pred=lowout8uk,mean.y.pred=meanout8uk,T=T,h=h))
(out12uk=strategy(spread=sp12ukout,up.y.pred=upout12uk,low.y.pred=lowout12uk,mean.y.pred=meanout12uk,T=T,h=h))
(out16uk=strategy(spread=sp16ukout,up.y.pred=upout16uk,low.y.pred=lowout16uk,mean.y.pred=meanout16uk,T=T,h=h))
(out20uk=strategy(spread=sp20ukout,up.y.pred=upout20uk,low.y.pred=lowout20uk,mean.y.pred=meanout20uk,T=T,h=h))

(out2uk_g=strategy(spread=sp2ukout,up.y.pred=upout2uk_g,low.y.pred=lowout2uk_g,mean.y.pred=meanout2uk_g,T=T,h=h))
(out8uk_g=strategy(spread=sp8ukout,up.y.pred=upout8uk_g,low.y.pred=lowout8uk_g,mean.y.pred=meanout8uk_g,T=T,h=h))
(out12uk_g=strategy(spread=sp12ukout,up.y.pred=upout12uk_g,low.y.pred=lowout12uk_g,mean.y.pred=meanout12uk_g,T=T,h=h))
(out16uk_g=strategy(spread=sp16ukout,up.y.pred=upout16uk_g,low.y.pred=lowout16uk_g,mean.y.pred=meanout16uk_g,T=T,h=h))
(out20uk_g=strategy(spread=sp20ukout,up.y.pred=upout20uk_g,low.y.pred=lowout20uk_g,mean.y.pred=meanout20uk_g,T=T,h=h))

(out2uk_bb=strategy(spread=spreadout2uk[20:length(spreadout3)],up.y.pred=spreadout2ukBB$up[20:length(spreadout2uk)],low.y.pred=spreadout2ukBB$dn[20:length(spreadout2uk)],mean.y.pred=spreadout2ukBB$mavg[20:length(spreadout2uk)],T=161,h=0))
(out8uk_bb=strategy(spread=spreadout8uk[20:length(spreadout8uk)],up.y.pred=spreadout8ukBB$up[20:length(spreadout8uk)],low.y.pred=spreadout8ukBB$dn[20:length(spreadout8uk)],mean.y.pred=spreadout8ukBB$mavg[20:length(spreadout8uk)],T=161,h=0))
(out12uk_bb=strategy(spread=spreadout12uk,up.y.pred=spreadout12ukBB$up[20:length(spreadout12uk)],low.y.pred=spreadout12ukBB$dn[20:length(spreadout12uk)],mean.y.pred=spreadout12ukBB$mavg[20:length(spreadout12uk)],T=161,h=0))
(out16uk_bb=strategy(spread=spreadout16uk,up.y.pred=spreadout16ukBB$up[20:length(spreadout16uk)],low.y.pred=spreadout16ukBB$dn[20:length(spreadout16uk)],mean.y.pred=spreadout16ukBB$mavg[20:length(spreadout16uk)],T=161,h=0))
(out20uk_bb=strategy(spread=spreadout20uk,up.y.pred=spreadout20ukBB$up[20:length(spreadout20uk)],low.y.pred=spreadout20ukBB$dn[20:length(spreadout20uk)],mean.y.pred=spreadout20ukBB$mavg[20:length(spreadout20uk)],T=161,h=0))

# Strategy 2
(out2.2uk=strategy2(spread=sp2ukout,up.y.pred=upout2uk,low.y.pred=lowout2uk,mean.y.pred=meanout2uk,T=T,h=h))
(out2.8uk=strategy2(spread=sp8ukout,up.y.pred=upout8uk,low.y.pred=lowout8uk,mean.y.pred=meanout8uk,T=T,h=h))
(out2.12uk=strategy2(spread=sp12ukout,up.y.pred=upout12uk,low.y.pred=lowout12uk,mean.y.pred=meanout12uk,T=T,h=h))
(out2.16uk=strategy2(spread=sp16ukout,up.y.pred=upout16uk,low.y.pred=lowout16uk,mean.y.pred=meanout16uk,T=T,h=h))
(out2.20uk=strategy2(spread=sp20ukout,up.y.pred=upout20uk,low.y.pred=lowout20uk,mean.y.pred=meanout20uk,T=T,h=h))

(out2.2uk_g=strategy2(spread=sp2ukout,up.y.pred=upout2uk_g,low.y.pred=lowout2uk_g,mean.y.pred=meanout2uk_g,T=T,h=h))
(out2.8uk_g=strategy2(spread=sp8ukout,up.y.pred=upout8uk_g,low.y.pred=lowout8uk_g,mean.y.pred=meanout8uk_g,T=T,h=h))
(out2.12uk_g=strategy2(spread=sp12ukout,up.y.pred=upout12uk_g,low.y.pred=lowout12uk_g,mean.y.pred=meanout12uk_g,T=T,h=h))
(out2.16uk_g=strategy2(spread=sp16ukout,up.y.pred=upout16uk_g,low.y.pred=lowout16uk_g,mean.y.pred=meanout16uk_g,T=T,h=h))
(out2.20uk_g=strategy2(spread=sp20ukout,up.y.pred=upout20uk_g,low.y.pred=lowout20uk_g,mean.y.pred=meanout20uk_g,T=T,h=h))

(out2.2uk_bb=strategy2(spread=spreadout2uk[20:length(spreadout2uk)],up.y.pred=spreadout2ukBB$up[20:length(spreadout2uk)],low.y.pred=spreadout2ukBB$dn[20:length(spreadout2uk)],mean.y.pred=spreadout2ukBB$mavg[20:length(spreadout2uk)],T=161,h=0))
(out2.8uk_bb=strategy2(spread=spreadout8uk[20:length(spreadout8uk)],up.y.pred=spreadout8ukBB$up[20:length(spreadout8uk)],low.y.pred=spreadout8ukBB$dn[20:length(spreadout8uk)],mean.y.pred=spreadout8ukBB$mavg[20:length(spreadout8uk)],T=161,h=0))
(out2.12uk_bb=strategy2(spread=spreadout12uk,up.y.pred=spreadout12ukBB$up[20:length(spreadout12uk)],low.y.pred=spreadout12ukBB$dn[20:length(spreadout12uk)],mean.y.pred=spreadout12ukBB$mavg[20:length(spreadout12uk)],T=161,h=0))
(out2.16uk_bb=strategy2(spread=spreadout16uk,up.y.pred=spreadout16ukBB$up[20:length(spreadout16uk)],low.y.pred=spreadout16ukBB$dn[20:length(spreadout16uk)],mean.y.pred=spreadout16ukBB$mavg[20:length(spreadout16uk)],T=161,h=0))
(out2.20uk_bb=strategy2(spread=spreadout20uk,up.y.pred=spreadout20ukBB$up[20:length(spreadout20uk)],low.y.pred=spreadout20ukBB$dn[20:length(spreadout20uk)],mean.y.pred=spreadout20ukBB$mavg[20:length(spreadout20uk)],T=161,h=0))

# Z-Score
os=2; cs=0.75; ol=-2; cl=-0.5

(outz.2uk=z.strategy(spread=spreadout2uk,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.8uk=z.strategy(spread=spreadout8uk,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.12uk=z.strategy(spread=spreadout12uk,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.16uk=z.strategy(spread=spreadout16uk,T=T,os=os,cs=cs,ol=ol,cl=cl))
(outz.20uk=z.strategy(spread=spreadout20uk,T=T,os=os,cs=cs,ol=ol,cl=cl))
