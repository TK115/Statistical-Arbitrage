### PREDICTIVE E(y_t+h|y_0:t)
smc2_pred_g<-function(N,T,y,sigma_rho,sigma_tau,sigma_sigma,h){
	start <- Sys.time ()
	
	yy=matrix(y[1:(T+1)],byrow=TRUE,nrow=N,ncol=T+1)
	w=matrix(1,nrow=N,ncol=T+1);ww=matrix(nrow=N,ncol=T+1)
	rho=vector("numeric"); sigma=vector("numeric"); tau=vector("numeric")
	rh=vector("numeric"); sig=vector("numeric"); ta=vector("numeric")
	loglik=vector("numeric")
	
	Array=array(0,dim=c(N,T+1,5)); Array_temp=array(0,dim=c(N,T+1+h,2))
	rho=runif(N,-1,1); sigma=rigamma(N,1,1); tau=rigamma(N,1,1)
	
	Array[,1,2]=rho; Array[,1,3]=sigma; Array[,1,4]=tau
	Array[,1,1]=rnorm(N); Array[,1,5]=Array[,1,1]+sigma*rnorm(N)
	Array_temp[,1,1]=Array[,1,1]; Array_temp[,1,2]=Array[,1,5]
	
	w[,1]=1/N
	ww[,1]=w[,1]/sum(w[,1])
	array_ind=sample(1:N,size=N,prob=ww[,1],replace=TRUE)
	Array[ ,1,]=Array[array_ind,1,]
	count=0
	
	for(i in 1:T){
		Array[,i+1,1]=rho*Array[,i,1]+tau*rnorm(N)
		Array[,i+1,5]=Array[,i+1,1]+sigma*rnorm(N)
		
		if(h>1){
			Array_temp[,i+1,1]=Array[,i+1,1]
			Array_temp[,i+1,2]=Array[,i+1,5]
			for(j in 1:(h-1)){
				Array_temp[,i+1+j,1]=rho*Array_temp[,i+j,1]+tau*rnorm(N)
				Array_temp[,i+1+j,2]=Array_temp[,i+1+j,1]+sigma*rnorm(N)
			}
			Array_temp[,i+1,1]=Array_temp[,i+h,1]
			Array_temp[,i+1,2]=Array_temp[,i+h,2]
		}
		
		Array[,i+1,2]=rho
		Array[,i+1,3]=sigma
		Array[,i+1,4]=tau
		w[,i+1]=dnorm(yy[,i+1],mean=Array[,i+1,1],sd=sigma)
		ww[,i+1]=w[,i+1]/sum(w[,i+1])
		
		if(1/sum(ww[,i+1]^2)<N/3){
			count=count+1
			gmhp=gmhp.one(rho_0=Array[,i+1,2],tau_0=Array[,i+1,4],sigma_0=Array[,i+1,3],sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,N=N,y=y)
			Array[,i+1,2]=gmhp[[1]] #rho
			Array[,i+1,3]=gmhp[[3]] #sigma
			Array[,i+1,4]=gmhp[[2]] #tau
			
			Array[,i+1,1]=Array[,i+1,2]*Array[,i,1]+Array[,i+1,4]*rnorm(N)
			w[,i+1]=dnorm(yy[,i+1],mean=Array[,i+1,1],sd=Array[,i+1,3])
			ww[,i+1]=w[,i+1]/sum(w[,i+1])	
		}
		
		array_ind=sample(1:N,size=N,prob=ww[,i+1],replace=TRUE)
		Array[ ,i+1,]=Array[array_ind,i+1,]
		
		rho=Array[,i+1,2]
		sigma=Array[,i+1,3]
		tau=Array[,i+1,4]
		
		#means for rho, sigma and tau
		rh[i]=mean(Array[,i,2])
		sig[i]=mean(Array[,i,3])
		ta[i]=mean(Array[,i,4])
		loglik[i]=sum(log(colMeans(w))) #p(y_[0:t])
		
		if(i%%20==0){
    			time=Sys.time () - start
    			cat("t=",i,", time spent=",time,"\n")
    		}
	}
	mean.x=colMeans(Array[,,1])
	if(h==1){
		mean.y.pred=colMeans(Array[,,5])
		low.y.pred=apply(Array[,,5],2,quantile,prob=0.05)
		up.y.pred=apply(Array[,,5],2,quantile,prob=0.95)
	}else{
		mean.y.pred=colMeans(Array_temp[,,2])[1:(T+1)]
		low.y.pred=apply(Array_temp[,,2],2,quantile,prob=0.05)[1:(T+1)]
		up.y.pred=apply(Array_temp[,,2],2,quantile,prob=0.95)[1:(T+1)]
	}
	p=log(colMeans(w)) #p(y_[t+1]|y_[0:t])
	return(list(rh,sig,ta,loglik,count,mean.x,mean.y.pred,low.y.pred,up.y.pred,p))
}

### ADDITIONAL FUNCTIONS
# calculating p with the Bootstrap Particle Filter (SIR) t dist innovations
p<-function(N,T,y,rho,tau,sigma){
	yy=matrix(y[1:(T+1)],byrow=TRUE,nrow=N,ncol=T+1)
	xx=matrix(nrow=N,ncol=T+1)
	w=matrix(nrow=N,ncol=T+1)
	ww=matrix(nrow=N,ncol=T+1)
	xx_res=matrix(nrow=N,ncol=T+1)
	
	xx[,1]=rnorm(N)
	w[,1]=1/N
	ww[,1]=w[,1]/sum(w[,1])
	xx_res[,1]=xx[,1]
	for(i in 1:T){
		xx[,i+1]=rho*xx_res[,i]+tau*rnorm(N)
		w[,i+1]=dnorm(yy[,i+1],mean=xx[,i+1],sd=sigma)	
		ww[,i+1]=w[,i+1]/sum(w[,i+1])
		#adaptive resampling
		if(1/sum(ww[,i+1]^2)>N/3) xx_res[,i+1]=xx[,i+1]
		else xx_res[,i+1]=sample(xx[,i+1],size=N,prob=ww[,i+1],replace=TRUE)
	}
	#p=prod(colMeans(w))
	p=exp(sum(log(colMeans(w))))
	return(p)
}

# one run of PMCMC
gmhp.one<-function(rho_0,tau_0,sigma_0,sigma_rho,sigma_tau,sigma_sigma,N,y){
	start <- Sys.time ()

	X=numeric(); Z=numeric(); V=numeric();
	X=rho_0; Z=tau_0; V=sigma_0 #rho, tau, sigma
	
    		W <- rlnorm(N,meanlog=log(Z)-sigma_tau^2/2,sdlog=sigma_tau) #NOT SYMMETRIC: proposal for tau
    		U1 <- runif(1)
    		if (U1<=min(p(rho=X,tau=W,N=N,T=length(y)-1,y=y,sigma=V)*dunif(X,min=-1,max=1)*densigamma(W,1,1)*densigamma(V,1,1)*dlnorm(W,meanlog=log(Z)-sigma_tau^2/2,sdlog=sigma_tau)/(p(rho=X,tau=Z,N=N,T=length(y)-1,y=y,sigma=V)*dunif(X,min=-1,max=1)*densigamma(Z,1,1)*densigamma(V,1,1)*dlnorm(Z,meanlog=log(W)-sigma_tau^2/2,sdlog=sigma_tau)),1)){ #tau
        		Z <- W
    		}else{
        		Z <- Z
    		}
    		
    		R <- rlnorm(N,meanlog=log(V)-sigma_sigma^2/2,sdlog=sigma_sigma)
 #NOT SYMMETRIC: proposal for sigma
    		U3 <- runif(1)
    		if (U3<=min(p(rho=X,tau=Z,N=N,T=length(y)-1,y=y,sigma=R)*dunif(X,min=-1,max=1)*densigamma(Z,1,1)*densigamma(R,1,1)*dlnorm(R,meanlog=log(V)-sigma_sigma^2/2,sdlog=sigma_sigma)/(p(rho=X,tau=Z,N=N,T=length(y)-1,y=y,sigma=V)*dunif(X,min=-1,max=1)*densigamma(Z,1,1)*densigamma(V,1,1)*dlnorm(V,meanlog=log(R)-sigma_sigma^2/2,sdlog=sigma_sigma)),1)){ #sigma
        		V <- R
    		}else{
        		V <- V
    		}	
    		
    		Y <- rtruncnorm(N,-1,1, mean=X,sd=sigma_rho) #SYMMETRIC: proposal for rho
    		U2<-runif(1)
    		if (U2<=min(p(tau=Z,rho=Y,N=N,T=length(y)-1,y=y,sigma=V)*dunif(Y,min=-1,max=1)*densigamma(Z,1,1)*densigamma(V,1,1)/(p(tau=Z,rho=X,N=N,T=length(y)-1,y=y,sigma=V)*dunif(X,min=-1,max=1)*densigamma(Z,1,1)*densigamma(V,1,1)),1)){ #rho
    	 		X <- Y
    		}else{
        		X <- X
    		}

	return(list(X,Z,V))
}
