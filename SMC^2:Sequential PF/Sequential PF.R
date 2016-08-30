rm(list=ls())
#set.seed(42)
install.packages("pscl")
install.packages("truncnorm")
library(pscl)
library(truncnorm)
library(coda)

### STOCHASTIC VOLATILITY MODEL
T=500
rho=0.91
sigma=1
beta=0.5

x=vector("numeric")
y=vector("numeric")
x[1]=rnorm(1,mean=0,sd=sqrt(sigma^2/(1-rho^2)))
for(n in 1:(T+1)){
  x[n+1]=rho*x[n]+sigma*rnorm(1)
  y[n]=beta*exp(x[n]/2)*rnorm(1)
}

### p(theta,x_[1:t]|y_[1:t])
p_seq_vol<-function(N,T){
	yy=matrix(y[1:(T+1)],byrow=TRUE,nrow=N,ncol=T+1)
	w=matrix(nrow=N,ncol=T+1);ww=matrix(nrow=N,ncol=T+1)
	rho=vector("numeric"); sigma=vector("numeric")
	rh=vector("numeric"); sig=vector("numeric")
	urh_c=vector("numeric"); usig_c=vector("numeric")
	lrh_c=vector("numeric"); lsig_c=vector("numeric")
	
	Array=array(0,c(N,T+1,3))
	rho=runif(N,-1,1)
	sigma=rigamma(N,1,1)
	
	Array[,1,1]=rnorm(N,mean=0,sd=sqrt(sigma^2/(1-rho^2)))
	Array[,1,2]=rho
	Array[,1,3]=sigma
	
	w[,1]=dnorm(yy[,1],mean=0,sd=beta*exp(Array[,1,1]/2))
	ww[,1]=w[,1]/sum(w[,1])
	array_ind=sample(1:N,size=N,prob=ww[,1],replace=TRUE)
	Array[ ,1,]=Array[array_ind,1,]
	
	for(i in 1:T){
		Array[,i+1,1]=rnorm(N,mean=rho*Array[,i,1],sd=sigma)
		Array[,i+1,2]=rho
		Array[,i+1,3]=sigma
		w[,i+1]=dnorm(yy[,i+1],mean=0,sd=beta*exp(Array[,i+1,1]/2))
		ww[,i+1]=w[,i+1]/sum(w[,i+1])
		array_ind=sample(1:N,size=N,prob=ww[,i+1],replace=TRUE)
		Array[ ,i+1,]=Array[array_ind,i+1,]
		rho=Array[,i+1,2]
		sigma=Array[,i+1,3]
		#medians for rho and sigma
		rh[i]=median(Array[,i,2])
		sig[i]=median(Array[,i,3])
		# 95% credible intervals for rho and sigma
		urh_c[i]=quantile(Array[,i,2],0.025)
		lrh_c[i]=quantile(Array[,i,2],0.975)
		usig_c[i]=quantile(Array[,i,3],0.975)
		lsig_c[i]=quantile(Array[,i,3],0.025)
	}
	return(list(rh,sig,urh_c,lrh_c,usig_c,lsig_c))
}

run=p_seq_vol(N=100000,T=500)

par(mfrow=c(1,2))
plot(run[[1]],ylab=expression(rho),xlab="Time",type="l")
lines(run[[3]],col="red")
lines(run[[4]],col="red")
plot(run[[2]],ylab=expression(sigma),xlab="Time",type="l")
lines(run[[5]],col="red")
lines(run[[6]],col="red")

##############################################################################

### LINEAR t-INNOVATIONS MODEL
rm(list=ls())

rho=0.8; tau=1; sigma=0.5; T=500; df=2

### Calculate y_[0:T] and trajectory x*_[0:T]
x=vector("numeric")
y=vector("numeric")
x[1]=rt(1,df=df)
for(n in 1:(T+1)){
	x[n+1]=rho*x[n]+tau*rt(1,df=df)
	y[n]=x[n]+sigma*rt(1,df=df)
}

### p(theta,x_[1:t]|y_[1:t])
p_seq_t<-function(N,T,y,df){
	yy=matrix(y[1:(T+1)],byrow=TRUE,nrow=N,ncol=T+1)
	w=matrix(1,nrow=N,ncol=T+1);ww=matrix(nrow=N,ncol=T+1)
	rho=vector("numeric"); sigma=vector("numeric"); tau=vector("numeric")
	rh=vector("numeric"); sig=vector("numeric"); ta=vector("numeric")
	loglik=vector("numeric")
	
	Array=array(0,dim=c(N,T+1,4)); rho=runif(N,-1,1)
	sigma=rigamma(N,1,1); tau=rigamma(N,1,1)
	
	Array[,1,1]=rt(N,df=df); Array[,1,2]=rho
	Array[,1,3]=sigma; Array[,1,4]=tau
	
	w[,1]=1/N
	ww[,1]=w[,1]/sum(w[,1])
	array_ind=sample(1:N,size=N,prob=ww[,1],replace=TRUE)
	Array[ ,1,]=Array[array_ind,1,]
	
	for(i in 1:T){
		Array[,i+1,1]=rho*Array[,i,1]+tau*rt(N,df=df)
		Array[,i+1,2]=rho
		Array[,i+1,3]=sigma
		Array[,i+1,4]=tau
		w[,i+1]=1/sigma*dt((yy[,i+1]-Array[,i+1,1])/sigma,df=df)
		ww[,i+1]=w[,i+1]/sum(w[,i+1])		
		array_ind=sample(1:N,size=N,prob=ww[,i+1],replace=TRUE)
		Array[ ,i+1,]=Array[array_ind,i+1,]
		rho=Array[,i+1,2]
		sigma=Array[,i+1,3]
		tau=Array[,i+1,4]
		
		#medians for rho and sigma
		rh[i]=median(Array[,i,2])
		sig[i]=median(Array[,i,3])
		ta[i]=median(Array[,i,4])
		loglik[i]=sum(log(colMeans(w)))
	}
	return(list(rh,sig,ta,loglik))
}

N=10000
run=p_seq_t(N=N,T=T,y=y,df=df)
rho_m=run[[1]]
sigma_m=run[[2]]
tau_m=run[[3]]
loglik=run[[4]]

par(mfrow=c(2,2))
plot(rho_m,ylab=expression(rho),xlab="Time",type="l")
abline(h=0.8,col="red")
plot(sigma_m,ylab=expression(sigma),xlab="Time",type="l")
abline(h=0.5,col="red")
plot(tau_m,ylab=expression(tau),xlab="Time",type="l")
abline(h=1,col="red")
plot(loglik,ylab=expression(log(p[y[0:t]])),xlab="Time",type="l")
