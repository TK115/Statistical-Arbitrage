# calculating p with the Bootstrap Particle Filter (SIR) t dist innovations
p<-function(N,T,y,rho,tau,sigma,df){
	yy=matrix(y[1:(T+1)],byrow=TRUE,nrow=N,ncol=T+1)
	xx=matrix(nrow=N,ncol=T+1)
	w=matrix(nrow=N,ncol=T+1)
	ww=matrix(nrow=N,ncol=T+1)
	xx_res=matrix(nrow=N,ncol=T+1)
	
	xx[,1]=rt(N,df=df)
	w[,1]=1/N
	ww[,1]=w[,1]/sum(w[,1])
	xx_res[,1]=xx[,1]
	for(i in 1:T){
		xx[,i+1]=rho*xx_res[,i]+tau*rt(N,df=df)
		w[,i+1]=1/sigma*dt((yy[,i+1]-xx[,i+1])/sigma,df=df)		
		ww[,i+1]=w[,i+1]/sum(w[,i+1])
		#adaptive resampling
		if(1/sum(ww[,i+1]^2)>N/3) xx_res[,i+1]=xx[,i+1]
		else xx_res[,i+1]=sample(xx[,i+1],size=N,prob=ww[,i+1],replace=TRUE)
	}
	#p=prod(colMeans(w))
	p=exp(sum(log(colMeans(w))))
	return(p)
}

gmhp<-function(rho_0,tau_0,sigma_rho,sigma_tau,nsteps,y){
	X=numeric(); Z=numeric(); lik=numeric()
	X[1]=rho_0; Z[1]=tau_0; #rho, tau
	lik[1]=p(rho=X[1],tau=Z[1],N=N,T=length(y)-1,y=y,sigma=sigma,df=df)
	for (i in 2:nsteps){
    		W <- rlnorm(1,meanlog=log(Z[i-1])-sigma_tau^2/2,sdlog=sigma_tau) #NOT SYMMETRIC: proposal for tau
    		U1 <- runif(1)
    		if (U1<=min(p(rho=X[i-1],tau=W,N=N,T=length(y)-1,y=y,sigma=sigma,df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(W,1,1)*dlnorm(W,meanlog=log(Z[i-1])-sigma_tau^2/2,sdlog=sigma_tau)/(p(rho=X[i-1],tau=Z[i-1],N=N,T=length(y)-1,y=y,sigma=sigma,df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(Z[i-1],1,1)*dlnorm(Z[i-1],meanlog=log(W)-sigma_tau^2/2,sdlog=sigma_tau)),1)){ #tau
        		Z[i] <- W
    		}else{
        		Z[i] <- Z[i-1]
    		}
    		Y <- rtruncnorm(1,-1,1, mean=X[i-1],sd=sigma_rho) #SYMMETRIC: proposal for rho
    		U2<-runif(1)
    		if (U2<=min(p(tau=Z[i],rho=Y,N=N,T=length(y)-1,y=y,sigma=sigma,df=df)*dunif(Y,min=-1,max=1)*densigamma(Z[i],1,1)/(p(tau=Z[i],rho=X[i-1],N=N,T=length(y)-1,y=y,sigma=sigma,df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(Z[i],1,1)),1)){ #rho
    	 		X[i] <- Y
    		}else{
        		X[i] <- X[i-1]
    		}
    		lik[i]=p(rho=X[i],tau=Z[i],N=N,T=length(y)-1,y=y,sigma=sigma,df=df)
	}
	return(list(X,Z,lik))
}

# full model for rho, tau, sigma
gmhp.full<-function(rho_0,tau_0,sigma_0,sigma_rho,sigma_tau,sigma_sigma,nsteps,y){
	start <- Sys.time ()

	X=numeric(); Z=numeric(); V=numeric(); lik=numeric()
	X[1]=rho_0; Z[1]=tau_0; V[1]=sigma_0 #rho, tau, sigma
	lik[1]=p(rho=X[1],tau=Z[1],N=N,T=length(y)-1,y=y,sigma=V[1],df=df)
	for (i in 2:nsteps){
    		W <- rlnorm(1,meanlog=log(Z[i-1])-sigma_tau^2/2,sdlog=sigma_tau) #NOT SYMMETRIC: proposal for tau
    		U1 <- runif(1)
    		if (U1<=min(p(rho=X[i-1],tau=W,N=N,T=length(y)-1,y=y,sigma=V[i-1],df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(W,1,1)*densigamma(V[i-1],1,1)*dlnorm(W,meanlog=log(Z[i-1])-sigma_tau^2/2,sdlog=sigma_tau)/(p(rho=X[i-1],tau=Z[i-1],N=N,T=length(y)-1,y=y,sigma=V[i-1],df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(Z[i-1],1,1)*densigamma(V[i-1],1,1)*dlnorm(Z[i-1],meanlog=log(W)-sigma_tau^2/2,sdlog=sigma_tau)),1)){ #tau
        		Z[i] <- W
    		}else{
        		Z[i] <- Z[i-1]
    		}
    		
    		R <- rlnorm(1,meanlog=log(V[i-1])-sigma_sigma^2/2,sdlog=sigma_sigma)
 #NOT SYMMETRIC: proposal for sigma
    		U3 <- runif(1)
    		if (U3<=min(p(rho=X[i-1],tau=Z[i],N=N,T=length(y)-1,y=y,sigma=R,df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(Z[i],1,1)*densigamma(R,1,1)*dlnorm(R,meanlog=log(V[i-1])-sigma_sigma^2/2,sdlog=sigma_sigma)/(p(rho=X[i-1],tau=Z[i],N=N,T=length(y)-1,y=y,sigma=V[i-1],df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(Z[i],1,1)*densigamma(V[i-1],1,1)*dlnorm(V[i-1],meanlog=log(R)-sigma_sigma^2/2,sdlog=sigma_sigma)),1)){ #sigma
        		V[i] <- R
    		}else{
        		V[i] <- V[i-1]
    		}	
    		
    		Y <- rtruncnorm(1,-1,1, mean=X[i-1],sd=sigma_rho) #SYMMETRIC: proposal for rho
    		U2<-runif(1)
    		if (U2<=min(p(tau=Z[i],rho=Y,N=N,T=length(y)-1,y=y,sigma=V[i],df=df)*dunif(Y,min=-1,max=1)*densigamma(Z[i],1,1)*densigamma(V[i],1,1)/(p(tau=Z[i],rho=X[i-1],N=N,T=length(y)-1,y=y,sigma=V[i],df=df)*dunif(X[i-1],min=-1,max=1)*densigamma(Z[i],1,1)*densigamma(V[i],1,1)),1)){ #rho
    	 		X[i] <- Y
    		}else{
        		X[i] <- X[i-1]
    		}
    		lik[i]=p(rho=X[i],tau=Z[i],N=N,T=length(y)-1,y=y,sigma=V[i],df=df)
    		if(i%%500==0){
    			time=Sys.time () - start
    			cat("PMCMC iteration=",i,", time spent=",time,"\n")
    		}
	}
	return(list(X,Z,V,lik))
}