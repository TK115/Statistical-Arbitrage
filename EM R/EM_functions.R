# Bootstrap Particle Filter (SIR) normal innovations
bpf_sir<-function(N,T,y,rho,tau,sigma){
	yy=matrix(y[1:(T+1)],byrow=TRUE,nrow=N,ncol=T+1)
	xx=matrix(nrow=N,ncol=T+1)
	w=matrix(nrow=N,ncol=T+1)
	ww=matrix(nrow=N,ncol=T+1)
	xx_res=matrix(nrow=N,ncol=T+1)
	
	xx[,1]=rnorm(N)
	w[,1]=1/N
	ww[,1]=w[,1]/sum(w[,1])
	xx_res[,1]=sample(xx[,1],size=N,prob=ww[,1],replace=TRUE)
	for(i in 1:T){
		xx[,i+1]=rho*xx_res[,i]+tau*rnorm(N)
		w[,i+1]=dnorm(yy[,i+1],mean=xx[,i+1],sd=sigma)
		ww[,i+1]=w[,i+1]/sum(w[,i+1])
		xx_res[,i+1]=sample(xx[,i+1],size=N,prob=ww[,i+1],replace=TRUE)
	}
	return(list(xx_res,sum(log(colMeans(w)))))
}

# Function for S1, S2, S3, S4 (E-Step)
S<-function(y,x,x0){
	S1=matrix(nrow=N,ncol=T+1); S2=matrix(nrow=N,ncol=T+1)
	S3=matrix(nrow=N,ncol=T+1); S4=matrix(nrow=N,ncol=T+1)
	S1[,1]=(y[1]-x[,1])^2; S2[,1]=x0^2; S3[,1]=x0*x[,1]; S4[,1]=x[,1]^2
	for(p in 2:(T+1)){
		S1[,p]=(y[p]-x[,p])^2
		S2[,p]=x[,p-1]^2
		S3[,p]=x[,p-1]*x[,p]
		S4[,p]=x[,p]^2
	}
	return(list(S1,S2,S3,S4))
}
