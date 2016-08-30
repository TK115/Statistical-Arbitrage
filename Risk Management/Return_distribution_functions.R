scenario.generator=function(type){
	y=vector("numeric")
	if(type=='WN') y=rnorm(T+1,mean=0,sd=sig_WN)
	else{
		x=vector("numeric")
		x[1]=sigma*rt(1,df=df)
		for(n in 1:(T+1)){
			x[n+1]=rho*x[n]+tau*rt(1,df=df)
			y[n]=x[n]+sigma*rt(1,df=df)
		}
	}	
	y
}


strategy.return.dist=function(n,type){
	start <- Sys.time ()
	return=vector("numeric"); return2=vector("numeric"); return.z=vector("numeric")
	return_risk=vector("numeric"); return2_risk=vector("numeric"); return.z_risk=vector("numeric")
	ADF=vector("numeric")
	for(i in 1:n){
		y=scenario.generator(T)
		ADF[i]=adf.test(y)$statistic
		run=smc2_pred(N=N,T=T,y=y,df=df,sigma_rho=sigma_rho,sigma_tau=sigma_tau,sigma_sigma=sigma_sigma,h=h,ci.l=ci.l,ci.u=ci.u,ess=ess)
		mean=run[[7]]
		low=run[[8]]
		up=run[[9]]

		return[i]=strategy(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h)[[1]]
		return2[i]=strategy2(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h)[[1]]
		return.z[i]=z.strategy(spread=y,T=T,os=os,cs=cs,ol=ol,cl=cl)[[1]]
		return_risk[i]=strategy_risk(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h,risk=risk)[[1]]
		return2_risk[i]=strategy2_risk(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h,risk=risk)[[1]]
		return.z_risk[i]=z.strategy_risk(spread=y,T=T,os=os,cs=cs,ol=ol,cl=cl,exit_short=exit_short,exit_long=exit_long)[[1]]
		
		if(i%%200==0){
    		time=Sys.time () - start
    		cat("i=",i,", time spent=",time,"\n")
    	}
	}
	return(list(return,return2,return.z,return_risk,return2_risk,return.z_risk,ADF))
}

### Bollinger Bands
strategy.return.dist.bb=function(n,type){
	start <- Sys.time ()
	h=0
	return=vector("numeric"); return2=vector("numeric"); return.z=vector("numeric")
	return_risk=vector("numeric"); return2_risk=vector("numeric"); return.z_risk=vector("numeric")
	ADF=vector("numeric")
	for(i in 1:n){
		y=scenario.generator(T)
		ADF[i]=adf.test(y)$statistic
		
		bby=BBands(y,n=20,maType=EMA,sd=2)
		y=y[20:T]
		ybb=data.frame(bby)
		
		
		mean=ybb$mavg[20:T]
		low=ybb$dn[20:T]
		up=ybb$up[20:T]
		T=T-19
		return[i]=strategy(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h)[[1]]*9/8
		return2[i]=strategy2(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h)[[1]]*9/8
		return.z[i]=z.strategy(spread=y,T=T,os=os,cs=cs,ol=ol,cl=cl)[[1]]*9/8
		return_risk[i]=strategy_risk(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h,risk=risk)[[1]]*9/8
		return2_risk[i]=strategy2_risk(spread=y,up.y.pred=up,low.y.pred=low,mean.y.pred=mean,T=T,h=h,risk=risk)[[1]]*9/8
		return.z_risk[i]=z.strategy_risk(spread=y,T=T,os=os,cs=cs,ol=ol,cl=cl,exit_short=exit_short,exit_long=exit_long)[[1]]*9/8
		
    	T=T+19
	}
	return(list(return,return2,return.z,return_risk,return2_risk,return.z_risk,ADF))
}
