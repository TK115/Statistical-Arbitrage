rm(list=ls())
#install.packages("R.matlab")
#install.packages("zoo")
#install.packages("tseries")
#install.packages("fUnitRoots")
#install.packages("urca")
#install.packages("vars")
library(tseries) #adf.test; kpss.test
library(fUnitRoots) #adfTest
library(urca) #ca.jo
library(vars)
library(R.matlab)
library(zoo)

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Cointegration")
source("Cointegration_functions.R")

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Spread")
source("Spread_functions_uk.R")

ukx <- readMat("ukx.mat")
ukximport <- ukx$pp 

#aquire data
Index=ukximport[1,1,1]$Index
tickers=ukximport[2,1,1]$tickers
names=ukximport[3,1,1]$names
sector=ukximport[4,1,1]$sector
dt=Matlab2Rdate(ukximport[5,1,1]$dt)
w=ukximport[6,1,1]$w
px=ukximport[7,1,1]$px
px.clean=ukximport[8,1,1]$px.clean

#calculate daily log-returns
ret=log(px.clean[2:dim(px.clean)[1],])-log(px.clean[1:(dim(px.clean)[1]-1),])
ret[is.na(ret)]=0
ret[is.infinite(ret)]=0

#days in each month
days=diff(seq(as.Date(dt[1]), as.Date(dt[length(dt)]), by = "month"))
days=c(days,19)
w=rbind(w,rep(0,ncol(w)))

####################### KEEP WEIGHTS SAME FOR ANY GIVEN YEAR #######################
years=rep(0,12)
years[1]=sum(days[1:12])
for(i in 2:11){
	years[i]=sum(days[1:(i*12)])-sum(days[1:((i-1)*12)])
}
years[12]=sum(days[133:134])

# January weights replicated over entire year
w.jan=matrix(nrow=12,ncol=dim(w)[2])
for(i in 1:12){
	w.jan[i,]=w[1+(i-1)*12,]
}

w.jan.days=w.jan[rep(1:nrow(w.jan), times = years), ]

#cumulative returns
index.ret2=vector("numeric"); cumul.index.ret2=vector("numeric")
cumul.index.ret2[1]=100
for(i in 1:nrow(ret)){
	index.ret2[i]=sum(ret[i,]*w.jan.days[i,])
	cumul.index.ret2[i+1]=cumul.index.ret2[i]*(1+index.ret2[i])
}

#30 stock in chosen index
index2.30=index.retN(30,w.jan.days,ret)
ret2.30=index2.30[[1]]
cumul2.30=index2.30[[2]]

#diagnostic check
sd(ret2.30-index.ret2)

par(mfrow=c(2,1))
ts.plot(index.ret2,main="FTSE 100 1.1.1990-19.3.2014",ylab="Daily log-returns of index")
lines(ret2.30,col="red",lty=3)
legend("bottomright",c("All stocks","Largest 30 stocks"),lty=c(1,3),col=c("black","red"),cex=0.5)
dev.off()

ts.plot(cumul.index.ret2,main="FTSE 100 1.1.1990-19.3.2014",ylab="Daily cumulative log-returns of index")
lines(cumul2.30,col="red",lty=3)
legend("bottomright",c("All stocks","Largest 30 stocks"),lty=c(1,3),col=c("black","red"),cex=0.5)
dev.off()

#check for cointegration
#entire period
create.spread.opt(cumul.index.ret2,cumul2.30,0.4,0.05)

#### check for all combinations
from2=4; to2=20; by2=4; days2=180; cor_thresh2=0.9; pval_thresh2=0.01
for(j in seq(from2,to2,by2)){
	cat("\nNumber of stocks=",j,"\n\n")
	index.j2=index.retN(j,w.jan.days,ret)[[2]]
	for(i in 1:floor(length(cumul.index.ret2)/days2)){
		spread2=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.j2[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[1]]
		print(c(spread2,i))
	}
	rm(spread2,index.j2,i,j)
}

### double check using Johansen Test
maxlag2=12*(days2/100)^0.25
for(j in seq(from2,to2,by2)){
	cat("\nNumber of stocks=",j,"\n\n")
	index.j2=index.retN(j,w.jan.days,ret)[[2]]
	for(i in 1:floor(length(cumul.index.ret2)/days2)){
		cumul.ret.all2=log(cumul.index.ret2[(days2*(i-1)):(days2*i)])
		cumul.ret.j2=log(index.j2[(days2*(i-1)):(days2*i)])
		data2=data.frame(cumul.ret.all2,cumul.ret.j2)
		
		varest2=VAR(data2,p=1,type="const",lag.max=maxlag2,ic="AIC")
		lagLength2=max(maxlag2,varest2$p)
		res2=ca.jo(data2,type="trace",ecdet="const",K=lagLength2,spec="transitory")
		testStatistics2=res2@teststat
		criticalValues2=res2@cval
		if(testStatistics2[length(testStatistics2)] >= criticalValues2[length(testStatistics2)+4]){
  			print(c(res2@V[1:(ncol(data2)+1),which.max(res2@lambda)],i))
  		}
	}
	rm(index.j2,cumul.ret.all2,cumul.ret.j2,data2,varest2,lagLength2,res2,testStatistics2,criticalValues2,i,j)
}

### optimal to use 8 stocks
index.8=index.retN(8,w.jan.days,ret)[[2]]
i=2
spreadin2uk=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.8[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockAin2uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBin2uk=index.8[(days2*(i-1)):(days2*i)]

i=1
spreadout2uk=log(index.8[(days2*(i-1)):(days2*i)])-0.5182355*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-2.2263834
spreadout2uk=c(0,spreadout2uk)
stockAout2uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBout2uk=index.8[(days2*(i-1)):(days2*i)]

ts.plot(spreadin2uk); lines(spreadout2uk,col="red")

###
i=8
spreadin8uk=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.8[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin8uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin8uk=index.8[(days2*(i-1)):(days2*i)]

i=7
spreadout8uk=log(index.8[(days2*(i-1)):(days2*i)])-0.3203663*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.1697717
stockBout8uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout8uk=index.8[(days2*(i-1)):(days2*i)]

ts.plot(spreadin8uk); lines(spreadout8uk,col="red")

###
i=12
spreadin12uk=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.8[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockAin12uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBin12uk=index.8[(days2*(i-1)):(days2*i)]

i=11
spreadout12uk=log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-2.044048*log(index.8[(days2*(i-1)):(days2*i)])+4.664923
stockAout12uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBout12uk=index.8[(days2*(i-1)):(days2*i)]

ts.plot(spreadin12uk); lines(spreadout12uk,col="red")

###
i=16
spreadin16uk=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.8[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin16uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin16uk=index.8[(days2*(i-1)):(days2*i)]

i=15
spreadout16uk=log(index.8[(days2*(i-1)):(days2*i)])-0.5207551*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-2.0980031
stockBout16uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout16uk=index.8[(days2*(i-1)):(days2*i)]

ts.plot(spreadin16uk); lines(spreadout16uk,col="red")

###
i=20
spreadin20uk=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.8[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin20uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin20uk=index.8[(days2*(i-1)):(days2*i)]

i=19
spreadout20uk=log(index.8[(days2*(i-1)):(days2*i)])-0.5160965*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-2.1093597
stockBout20uk=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout20uk=index.8[(days2*(i-1)):(days2*i)]

ts.plot(spreadin20uk); lines(spreadout20uk,col="red")

rm(i)

par(mfrow=c(4,3))
ts.plot(spreadin2uk,xlab="Time (in days)",ylab="In Sample",main=expression(paste("2003, ",beta,"=(-0.5182355,1)",", u=-2.2263834")))
ts.plot(spreadin8uk,xlab="Time (in days)",ylab="",main=expression(paste("2006, ",beta,"=(-0.3203663,1)",", u=-3.1697717")))
ts.plot(spreadin12uk,xlab="Time (in days)",ylab="",main=expression(paste("2008, ",beta,"=(1,-2.044048)",", u=4.664923")))
ts.plot(spreadout2uk,xlab="Time (in days)",col="red",ylab="Out Sample",main=expression(paste("2003, ",beta,"=(-0.5182355,1)",", u=-2.2263834")))
ts.plot(spreadout8uk,xlab="Time (in days)",col="red",ylab="",main=expression(paste("2006, ",beta,"=(-0.3203663,1)",", u=-3.1697717")))
ts.plot(spreadout12uk,xlab="Time (in days)",col="red",ylab="",main=expression(paste("2008, ",beta,"=(1,-2.044048)",", u=4.664923")))
ts.plot(spreadin16uk,xlab="Time (in days)",ylab="In Sample",main=expression(paste("2011, ",beta,"=(-0.5207551,1)",", u=-2.0980031")))
ts.plot(spreadin20uk,xlab="Time (in days)",ylab="",main=expression(paste("2013, ",beta,"=(-0.5160965,1)",", u=-2.1093597")))
ts.plot(spreadout16uk,xlab="Time (in days)",col="red",ylab="Out Sample",main=expression(paste("2011, ",beta,"=(-0.5207551,1)",", u=-2.0980031")))
ts.plot(spreadout20uk,xlab="Time (in days)",col="red",ylab="",main=expression(paste("2013, ",beta,"=(-0.5160965,1)",", u=-2.1093597")))
dev.off()
