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
source("Spread_functions.R")

spx <- readMat("spx.mat")
spximport <- spx$pp 

#aquire data
tickers=spximport[1,1,1]$tickers
names=spximport[2,1,1]$names
sector=spximport[3,1,1]$sector
dt=Matlab2Rdate(spximport[4,1,1]$dt)
Index=spximport[5,1,1]$Index
w=spximport[6,1,1]$w
px=spximport[7,1,1]$px
px.clean=spximport[8,1,1]$px.clean

#calculate daily log-returns
ret=log(px.clean[2:dim(px.clean)[1],])-log(px.clean[1:(dim(px.clean)[1]-1),])
ret[is.na(ret)]=0
ret[is.infinite(ret)]=0

#days in each month
days=diff(seq(as.Date(dt[1]), as.Date(dt[length(dt)]), by = "month"))
days=c(days,19)
w=rbind(w,rep(0,ncol(w)))
w.days=w[rep(1:nrow(w), times = days), ]

#stock index daily returns
ret=rbind(rep(0,ncol(ret)),ret)

index.ret=vector("numeric"); cumul.index.ret=vector("numeric")
cumul.index.ret[1]=100
for(i in 1:nrow(ret)){
	index.ret[i]=sum(ret[i,]*w.days[i,])
	cumul.index.ret[i+1]=cumul.index.ret[i]*(1+index.ret[i])
}

#100 stock in chosen index
index100=index.retN(100,w.days,ret)
ret100=index100[[1]]
cumul100=index100[[2]]

#diagnostic check
sd(ret100-index.ret)

par(mfrow=c(2,1))
ts.plot(index.ret,main="S&P 500 1.1.1990-19.3.2014",ylab="Daily log-returns of index")
lines(ret100,col="red",lty=3)
legend("bottomright",c("All stocks","Largest 100 stocks"),lty=c(1,3),col=c("black","red"),cex=0.5)

ts.plot(cumul.index.ret,main="S&P 500 1.1.1990-19.3.2014",ylab="Daily cumulative log-returns of index")
lines(cumul100,col="red",lty=3)
legend("bottomright",c("All stocks","Largest 100 stocks"),lty=c(1,3),col=c("black","red"),cex=0.5)
dev.off()

#check for cointegration
#entire period
create.spread.opt(cumul.index.ret,cumul100,0.4,0.05)

#### check for all combinations
from=10; to=40; by=5; dayss=180; cor_thresh=0.9; pval_thresh=0.01
for(j in seq(from,to,by)){
	cat("\nNumber of stocks=",j,"\n\n")
	index.j=index.retN(j,w.days,ret)[[2]]
	for(i in 1:floor(length(cumul.index.ret)/dayss)){
		spread=create.spread.opt(cumul.index.ret[(dayss*(i-1)):(dayss*i)],index.j[(dayss*(i-1)):(dayss*i)],cor_thresh,pval_thresh)[[1]]
		print(c(spread,i))
	}
	rm(spread,index.j)
}

### double check using Johansen Test
maxlag=12*(dayss/100)^0.25
for(j in seq(from,to,by)){
	cat("\nNumber of stocks=",j,"\n\n")
	index.j=index.retN(j,w.days,ret)[[2]]
	for(i in 1:floor(length(cumul.index.ret)/dayss)){
		cumul.ret.all=log(cumul.index.ret[(dayss*(i-1)):(dayss*i)])
		cumul.ret.j=log(index.j[(dayss*(i-1)):(dayss*i)])
		data=data.frame(cumul.ret.all,cumul.ret.j)
		
		varest=VAR(data,p=1,type="const",lag.max=maxlag,ic="AIC")
		lagLength=max(maxlag,varest$p)
		res=ca.jo(data,type="trace",ecdet="const",K=lagLength,spec="transitory")
		testStatistics=res@teststat
		criticalValues=res@cval
		if(testStatistics[length(testStatistics)] >= criticalValues[length(testStatistics)+4]){
  			print(c(res@V[1:(ncol(data)+1),which.max(res@lambda)],i))
  		}
	}
	rm(index.j,cumul.ret.all,cumul.ret.j,data,varest,lagLength,res,testStatistics,criticalValues)
}

####################### KEEP WEIGHTS SAME FOR ANY GIVEN YEAR #######################
years=rep(0,25)
years[1]=sum(days[1:12])
for(i in 2:24){
	years[i]=sum(days[1:(i*12)])-sum(days[1:((i-1)*12)])
}
years[25]=sum(days[289:291])

# January weights replicated over entire year
w.jan=matrix(nrow=25,ncol=dim(w)[2])
for(i in 1:25){
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

#100 stock in chosen index
index2.100=index.retN(100,w.jan.days,ret)
ret2.100=index2.100[[1]]
cumul2.100=index2.100[[2]]

#diagnostic check
sd(ret2.100-index.ret2)

par(mfrow=c(2,1))
ts.plot(index.ret2,main="S&P 500 1.1.1990-19.3.2014",ylab="Daily log-returns of index")
lines(ret2.100,col="red",lty=3)
legend("bottomright",c("All stocks","Largest 100 stocks"),lty=c(1,3),col=c("black","red"),cex=0.5)

ts.plot(cumul.index.ret2,main="S&P 500 1.1.1990-19.3.2014",ylab="Daily cumulative log-returns of index")
lines(cumul2.100,col="red",lty=3)
legend("bottomright",c("All stocks","Largest 100 stocks"),lty=c(1,3),col=c("black","red"),cex=0.5)
dev.off()

ret.index.20=index.retN(20,w.jan.days,ret)[[1]]
#mean(index.ret2); sd(index.ret2)
d1=rnorm(1e7,mean=mean(index.ret2),sd=sd(index.ret2))
d2=sd(index.ret2)*rt(1e7,df=5)+mean(index.ret2)
d3=sd(index.ret2)*rt(1e7,df=4)+mean(index.ret2)
d4=sd(index.ret2)*rt(1e7,df=3)+mean(index.ret2)
d5=sd(index.ret2)*rt(1e7,df=2)+mean(index.ret2)
d6=sd(index.ret2)*rt(1e7,df=1)+mean(index.ret2)
#mean(ret.index.20); sd(ret.index.20)
d7=rnorm(1e7,mean=mean(ret.index.20),sd=sd(ret.index.20))
d8=sd(ret.index.20)*rt(1e7,df=5)+mean(ret.index.20)
d9=sd(ret.index.20)*rt(1e7,df=4)+mean(ret.index.20)
d10=sd(ret.index.20)*rt(1e7,df=3)+mean(ret.index.20)
d11=sd(ret.index.20)*rt(1e7,df=2)+mean(ret.index.20)
d12=sd(ret.index.20)*rt(1e7,df=1)+mean(ret.index.20)

par(mfrow=c(1,2))
hist(index.ret2,breaks=100,freq=FALSE)
lines(density(d1),col="red")
lines(density(d2),col="green")
lines(density(d3),col="green")
lines(density(d4),col="green")
lines(density(d5),col="green")
lines(density(d6),col="green")
hist(ret.index.20,breaks=100,freq=FALSE)
lines(density(d7),col="red")
lines(density(d8),col="green")
lines(density(d9),col="green")
lines(density(d10),col="green")
lines(density(d11),col="green")
lines(density(d12),col="green")
dev.off()

#check for cointegration
#entire period
create.spread.opt(cumul.index.ret,cumul100,0.4,0.05)

#### check for all combinations
from2=10; to2=40; by2=5; days2=180; cor_thresh2=0.9; pval_thresh2=0.01
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

### optimal to use 20 stocks
index.20=index.retN(20,w.jan.days,ret)[[2]]
i=3
spreadin3=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.20[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockAin3=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBin3=index.20[(days2*(i-1)):(days2*i)]

i=4
spreadout3=log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.290711*log(index.20[(days2*(i-1)):(days2*i)])+10.642420
stockAout3=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBout3=index.20[(days2*(i-1)):(days2*i)]

ts.plot(spreadin3); lines(spreadout3,col="red")

###
i=9
spreadin9=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.20[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin9=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin9=index.20[(days2*(i-1)):(days2*i)]

i=10
spreadout9=log(index.20[(days2*(i-1)):(days2*i)])-0.2673692*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.3771963
stockBout9=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout9=index.20[(days2*(i-1)):(days2*i)]

ts.plot(spreadin9); lines(spreadout9,col="red")

###
i=11
spreadin11=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.20[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockAin11=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBin11=index.20[(days2*(i-1)):(days2*i)]

i=12
spreadout11=log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.160233*log(index.20[(days2*(i-1)):(days2*i)])+9.950650
stockAout11=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockBout11=index.20[(days2*(i-1)):(days2*i)]

ts.plot(spreadin11); lines(spreadout11,col="red")

###
i=17
spreadin17=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.20[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin17=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin17=index.20[(days2*(i-1)):(days2*i)]

i=18
spreadout17=log(index.20[(days2*(i-1)):(days2*i)])-0.2716707*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.4214602
stockBout17=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout17=index.20[(days2*(i-1)):(days2*i)]

ts.plot(spreadin17); lines(spreadout17,col="red")

###
i=31
spreadin31=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.20[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin31=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin31=index.20[(days2*(i-1)):(days2*i)]

i=32
spreadout31=log(index.20[(days2*(i-1)):(days2*i)])-0.2855611*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.3344949
stockBout31=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout31=index.20[(days2*(i-1)):(days2*i)]

ts.plot(spreadin31); lines(spreadout31,col="red")

###
i=45
spreadin45=create.spread.opt(cumul.index.ret2[(days2*(i-1)):(days2*i)],index.20[(days2*(i-1)):(days2*i)],cor_thresh2,pval_thresh2)[[2]]
stockBin45=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAin45=index.20[(days2*(i-1)):(days2*i)]

i=46
spreadout45=log(index.20[(days2*(i-1)):(days2*i)])-0.2845362*log(cumul.index.ret2[(days2*(i-1)):(days2*i)])-3.3266583
stockBout45=cumul.index.ret2[(days2*(i-1)):(days2*i)]
stockAout45=index.20[(days2*(i-1)):(days2*i)]

ts.plot(spreadin45); lines(spreadout45,col="red")
rm(i)

par(mfrow=c(4,3))
ts.plot(spreadin3,xlab="Time (in days)",ylab="In Sample",main=expression(paste("1991, ",beta,"=(1,-3.290711)",", u=10.642420")))
ts.plot(spreadin9,xlab="Time (in days)",ylab="",main=expression(paste("1995, ",beta,"=(-0.2673692,1)",", u=-3.3771963")))
ts.plot(spreadin11,xlab="Time (in days)",ylab="",main=expression(paste("1996, ",beta,"=(1,-3.160233)",", u=9.950650")))
ts.plot(spreadout3,col="red",xlab="Time (in days)",ylab="Out Sample",main=expression(paste("1991, ",beta,"=(1,-3.290711)",", u=10.642420")))
ts.plot(spreadout9,col="red",xlab="Time (in days)",ylab="",main=expression(paste("1995, ",beta,"=(-0.2673692,1)",", u=-3.3771963")))
ts.plot(spreadout11,col="red",xlab="Time (in days)",ylab="",main=expression(paste("1996, ",beta,"=(1,-3.160233)",", u=9.950650")))
ts.plot(spreadin17,xlab="Time (in days)",ylab="In Sample",main=expression(paste("1999, ",beta,"=(-0.2716707,1)",", u=-3.4214602")))
ts.plot(spreadin31,xlab="Time (in days)",ylab="",main=expression(paste("2006, ",beta,"=(-0.2855611,1)",", u=-3.3344949")))
ts.plot(spreadin45,xlab="Time (in days)",ylab="",main=expression(paste("2013, ",beta,"=(-0.2845362,1)",", u=-3.3266583")))
ts.plot(spreadout17,col="red",xlab="Time (in days)",ylab="Out Sample",main=expression(paste("1999, ",beta,"=(-0.2716707,1)",", u=-3.4214602")))
ts.plot(spreadout31,col="red",xlab="Time (in days)",ylab="",main=expression(paste("2006, ",beta,"=(-0.2855611,1)",", u=-3.3344949")))
ts.plot(spreadout45,col="red",xlab="Time (in days)",ylab="",main=expression(paste("2013, ",beta,"=(-0.2845362,1)",", u=-3.3266583")))
dev.off()
