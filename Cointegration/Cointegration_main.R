#install.packages("quantmod")
#install.packages("tseries")
#install.packages("fUnitRoots")
#install.packages("Quandl")
#install.packages("urca")
#install.packages("vars")
#install.packages("tsDyn")
#install.packages("HAC")
library(quantmod)
library(tseries) #adf.test; kpss.test
library(fUnitRoots) #adfTest
library(Quandl)
library(urca) #ca.jo
library(vars)
library(tsDyn) #better Johansen cointegration test
library(HAC) #HAC regression

setwd("/Users/tillkischkat/Desktop/MSc Statistics/Trimester 3/Statistical Arbitrage/R:MATLAB Code/Cointegration")

rm(list=ls())
AAPL<-Quandl("WIKI/AAPL")
IBM<-Quandl("WIKI/IBM")
MSFT<-Quandl("WIKI/MSFT")
COKE<-Quandl("WIKI/COKE")
PEP<-Quandl("WIKI/PEP")
AAPL.close<-AAPL$Close
IBM.close<-IBM$Close
MSFT.close<-MSFT$Close
COKE.close<-COKE$Close
PEP.close<-PEP$Close

par(mfrow=c(2,1))
ts.plot(AAPL.close,ylab="AAPL vs. IBM")
lines(IBM.close,col="red")
ts.plot(COKE.close,ylab="COKE vs. PEP")
lines(PEP.close,col="red")
dev.off()

source("Cointegration_functions.R")
list1.1=create.spread(AAPL.close,IBM.close,0.6,0.01)
list1.2=create.spread(IBM.close,AAPL.close,0.6,0.01)
list2.1=create.spread(COKE.close,PEP.close,0.6,0.01)
list2.2=create.spread(PEP.close,COKE.close,0.6,0.01)

par(mfrow=c(2,1))
ts.plot(list1.1[[6]],ylab="",main="APPL/IBM Spread")
lines(list1.2[[6]],ylab="",col="red")
ts.plot(list2.1[[6]],ylab="",main="COKE/PEP Spread")
lines(list2.2[[6]],ylab="",col="red")
dev.off()

##############
list3=create.spread.opt(AAPL.close,IBM.close,0.6,0.01)
list4=create.spread.opt(PEP.close,COKE.close,0.6,0.01)
list5=create.spread.opt(PEP.close,AAPL.close,0.1,0.01)

par(mfrow=c(2,1))
ts.plot(list3[[2]],ylab="",main="APPL/IBM Spread")
ts.plot(list4[[2]],ylab="",main="COKE/PEP Spread")
dev.off()

###################################################

for(i in 1:40){
	a=create.spread.opt(IBM.close[(170*(i-1)):(170*i)],AAPL.close[(170*(i-1)):(170*i)],0.2,0.01)[[1]]
	print(c(a,i))
}
# potential cointigration for i=11

for(i in 1:40){
	a=create.spread.opt(IBM.close[(170*(i-1)):(170*i)],MSFT.close[(170*(i-1)):(170*i)],0.2,0.05)[[1]]
	print(c(a,i))
}
# potential cointigration for i=10

for(i in 1:40){
	a=create.spread.opt(AAPL.close[(170*(i-1)):(170*i)],MSFT.close[(170*(i-1)):(170*i)],0.2,0.05)[[1]]
	print(c(a,i))
}
# potential cointigration for i=7

for(i in 1:30){
	a=create.spread.opt(PEP.close[(170*(i-1)):(170*i)],COKE.close[(170*(i-1)):(170*i)],0.2,0.01)[[1]]
	print(c(a,i))
}
# potential cointigration for i=2

################################ JOHANSEN ###################################
apple=log(AAPL.close)
ibm=log(IBM.close)
ibm=ibm[1:length(apple)]
data=cbind(apple,ibm)

varest <- VAR(data,p=1,type="const",lag.max=(12*(length(ibm)/100)^0.25), ic="AIC")
# obtain lag length of VAR that best fits the data
lagLength <- max(2,varest$p)

# Perform Johansen procedure for cointegration
# Allow intercepts in the cointegrating vector: data without zero mean
# Use trace statistic (null hypothesis: number of cointegrating vectors <= r)
res <- ca.jo(data,type="trace",ecdet="const",K=lagLength,spec="transitory")
summary(res)

# rank.test
rank.test(VECM(data,lag=lagLength,estim="ML"), type = "eigen", cval = 0.05)
rank.test(VECM(data,lag=lagLength,estim="ML"), type = "eigen", r_null=1,cval = 0.05)
############################################################################

coke=log(COKE.close)
pepsi=log(PEP.close)
pepsi=pepsi[1:length(coke)]

varest2 <- VAR(data.frame(coke,pepsi),p=1,type="const",lag.max=(12*(length(coke)/100)^0.25), ic="SC")
lagLength2 <- max(2,varest2$p)
res2 <- ca.jo(data.frame(coke,pepsi),type="trace",ecdet="const",K=lagLength2,spec="longrun")
summary(res2)

### EXTRA
testStatistics <- res@teststat
criticalValues <- res@cval

# chi^2. If testStatic for r<= 0 is greater than the corresponding criticalValue, then r<=0 is rejected and we have at least one cointegrating vector
# We use 90% confidence level to make our decision
if(testStatistics[length(testStatistics)] >= criticalValues[length(testStatistics)+2])
{
# Return eigenvector that has maximum eigenvalue. Note: we throw away the constant!!
  return(res@V[1:(ncol(data)+1),which.max(res@lambda)])
}