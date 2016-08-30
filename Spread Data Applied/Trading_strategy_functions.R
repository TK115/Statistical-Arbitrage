### TRADING STRATEGY FUNCTION: Mean-Reversion
strategy=function(spread,up.y.pred,low.y.pred,mean.y.pred,T,h=h){
	port=vector("numeric")
	port[1]=0
	
	count_short=0; count_long=0; signal=0; ret=0; trades=0
	for(t in 2:T){
		if(((spread[h+t]<up.y.pred[t] && spread[h+t-1]>up.y.pred[t-1]) || signal==1) && signal!=2){
			signal=1
			if(spread[h+t]>mean.y.pred[t]){
				port[t]=spread[h+t]
				count_short=count_short+1
			}else{
				port[t]=spread[h+t]
				#ret=ret+spread[h+t-count_short]-spread[h+t]
				ret=ret+port[t-count_short]-port[t]
				signal=0
				count_short=0
				trades=trades+1
			}
		}else if(((spread[h+t]>low.y.pred[t] && spread[h+t-1]<low.y.pred[t-1]) || signal==2) && signal!=1){
			signal=2
			if(spread[t]<mean.y.pred[t-h]){
				port[t]=-spread[h+t]
				count_long=count_long+1
			}else{
				port[t]=-spread[h+t]
				#ret=ret+spread[h+t]-spread[h+t-count_long]
				ret=ret+port[t-count_long]-port[t]
				count_long=0
				signal=0
				trades=trades+1
			}
		}else{
			port[t]=0
		}
	}
	if(port[T]!=0 && signal==1) ret=ret+port[T-count_short]-port[T]
	if(port[T]!=0 && signal==2) ret=ret+port[T-count_long]-port[T]
	return(list(ret,trades,sd(port)))
}

### TRADING STRATEGY: Band-Reversion
strategy2=function(spread,up.y.pred,low.y.pred,mean.y.pred,T,h=h){
	port=vector("numeric")
	port[1]=0
	
	count_short=0; count_long=0; signal=0; ret=0; trades=0
	for(t in 2:T){
		if(((spread[h+t]<up.y.pred[t] && spread[h+t-1]>up.y.pred[t-1]) || signal==1) && signal!=2){
			signal=1
			if(spread[h+t]>low.y.pred[t]){
				port[t]=spread[h+t]
				count_short=count_short+1
			}else{
				port[t]=spread[h+t]
				#ret=ret+spread[h+t-count_short]-spread[h+t]
				ret=ret+port[t-count_short]-port[t]
				signal=0
				count_short=0
				trades=trades+1
			}
		}else if(((spread[h+t]>low.y.pred[t] && spread[h+t-1]<low.y.pred[t-1]) || signal==2) && signal!=1){
			signal=2
			if(spread[t]<up.y.pred[t-h]){
				port[t]=-spread[h+t]
				count_long=count_long+1
			}else{
				port[t]=-spread[h+t]
				#ret=ret+spread[h+t]-spread[h+t-count_long]
				ret=ret+port[t-count_long]-port[t]
				count_long=0
				signal=0
				trades=trades+1
			}
		}else{
			port[t]=0
		}
	}
	if(port[T]!=0 && signal==1) ret=ret+port[T-count_short]-port[T]
	if(port[T]!=0 && signal==2) ret=ret+port[T-count_long]-port[T]
	return(list(ret,trades,sd(port)))
}

### TRADING STRATEGY FUNCTION: Z-Score
z.strategy=function(spread,T,os,cs,ol,cl){
	port=vector("numeric")
	
	count_short=0; count_long=0; signal=0; ret=0; trades=0
	z=(spread-mean(spread))/sd(spread)
	for(t in 2:T){
		if(z[t]>os || signal==1){
			signal=1
			if(z[t]>cs){
				port[t]=spread[t]
				count_short=count_short+1
			}else{
				port[t]=spread[t]
				#ret=ret+spread[t-count_short]-spread[t]
				ret=ret+port[t-count_short]-port[t]
				signal=0
				count_short=0
				trades=trades+1
			}
		}else if(z[t]<ol || signal==2){
			signal=2
			if(z[t]<cl){
				port[t]=-spread[t]
				count_long=count_long+1
			}else{
				port[t]=-spread[t]
				#ret=ret+spread[t]-spread[t-count_long]
				ret=ret+port[t-count_long]-port[t]
				count_long=0
				signal=0
				trades=trades+1
			}
		}else{
			port[t]=0
		}
	}
	if(port[T]!=0 && signal==1) ret=ret+port[T-count_short]-port[T]
	if(port[T]!=0 && signal==2) ret=ret+port[T-count_long]-port[T]
	return(list(ret,trades,sd(port[2:length(port)])))
}


################################ RISK MANAGEMENT VIEW ###################################
#MAXIMUM DRAWDOWN
mdd=function(y,t){
	max(y[1:t])-min(y[1:t])
}

### TRADING STRATEGY FUNCTION: Z-Score (with risk management)
z.strategy_risk=function(spread,T,os,cs,ol,cl,exit_short,exit_long){
	port=vector("numeric")
	
	count_short=0; count_long=0; signal=0; ret=0; trades=0
	z=(spread-mean(spread))/sd(spread)
	for(t in 2:T){
		if(z[t]>os || signal==1){
			signal=1
			if(z[t]>cs){
				port[t]=spread[t]
				count_short=count_short+1
			}else{
				port[t]=spread[t]
				#ret=ret+spread[t-count_short]-spread[t]
				ret=ret+port[t-count_short]-port[t]
				signal=0
				count_short=0
				trades=trades+1
			}
		}else if(z[t]<ol || signal==2){
			signal=2
			if(z[t]<cl){
				port[t]=-spread[t]
				count_long=count_long+1
			}else{
				port[t]=-spread[t]
				#ret=ret+spread[t]-spread[t-count_long]
				ret=ret+port[t-count_long]-port[t]
				count_long=0
				signal=0
				trades=trades+1
			}
		}else{
			port[t]=0
		}
		if(z[t]>exit_short){
			ret=ret+port[t-count_short]-port[t]
			return(list(ret,trades,sd(port)))
		}
		if(z[t]<exit_long){
			ret=ret+port[t-count_long]-port[t]
			return(list(ret,trades,sd(port)))
		}
	}
	if(port[T]!=0 && signal==1) ret=ret+port[T-count_short]-port[T]
	if(port[T]!=0 && signal==2) ret=ret+port[T-count_long]-port[T]
	return(list(ret,trades,sd(port)))
}

### TRADING STRATEGY: Mean-Reversion (with risk management)
strategy_risk=function(spread,up.y.pred,low.y.pred,mean.y.pred,T,h=h,risk){
	port=vector("numeric")
	port[1]=0
	
	count_short=0; count_long=0; signal=0; ret=0; trades=0; ret_risk=0
	for(t in 2:T){
		if(((spread[h+t]<up.y.pred[t] && spread[h+t-1]>up.y.pred[t-1]) || signal==1) && signal!=2){
			signal=1
			if(spread[h+t]>mean.y.pred[t]){
				port[t]=spread[h+t]
				count_short=count_short+1
			}else{
				port[t]=spread[h+t]
				#ret=ret+spread[h+t-count_short]-spread[h+t]
				ret=ret+port[t-count_short]-port[t]
				signal=0
				count_short=0
				trades=trades+1
			}
		}else if(((spread[h+t]>low.y.pred[t] && spread[h+t-1]<low.y.pred[t-1]) || signal==2) && signal!=1){
			signal=2
			if(spread[t]<mean.y.pred[t-h]){
				port[t]=-spread[h+t]
				count_long=count_long+1
			}else{
				port[t]=-spread[h+t]
				#ret=ret+spread[h+t]-spread[h+t-count_long]
				ret=ret+port[t-count_long]-port[t]
				count_long=0
				signal=0
				trades=trades+1
			}
		}else{
			port[t]=0
		}
		if(signal==1) ret_risk=ret+port[t-count_short]-port[t]
		if(signal==2) ret_risk=ret+port[t-count_long]-port[t]
		
		if(ret_risk<risk){
			ret=ret_risk
			return(list(ret,trades,sd(port)))
		}
	}
	if(port[T]!=0 && signal==1) ret=ret+port[T-count_short]-port[T]
	if(port[T]!=0 && signal==2) ret=ret+port[T-count_long]-port[T]
	return(list(ret,trades,sd(port)))
}


### TRADING STRATEGY: Band-Reversion (with risk management)
strategy2_risk=function(spread,up.y.pred,low.y.pred,mean.y.pred,T,h=h,risk){
	port=vector("numeric")
	port[1]=0
	
	count_short=0; count_long=0; signal=0; ret=0; trades=0; ret_risk=0
	for(t in 2:T){
		if(((spread[h+t]<up.y.pred[t] && spread[h+t-1]>up.y.pred[t-1]) || signal==1) && signal!=2){
			signal=1
			if(spread[h+t]>low.y.pred[t]){
				port[t]=spread[h+t]
				count_short=count_short+1
			}else{
				port[t]=spread[h+t]
				#ret=ret+spread[h+t-count_short]-spread[h+t]
				ret=ret+port[t-count_short]-port[t]
				signal=0
				count_short=0
				trades=trades+1
			}
		}else if(((spread[h+t]>low.y.pred[t] && spread[h+t-1]<low.y.pred[t-1]) || signal==2) && signal!=1){
			signal=2
			if(spread[t]<up.y.pred[t-h]){
				port[t]=-spread[h+t]
				count_long=count_long+1
			}else{
				port[t]=-spread[h+t]
				#ret=ret+spread[h+t]-spread[h+t-count_long]
				ret=ret+port[t-count_long]-port[t]
				count_long=0
				signal=0
				trades=trades+1
			}
		}else{
			port[t]=0
		}
		if(signal==1) ret_risk=ret+port[t-count_short]-port[t]
		if(signal==2) ret_risk=ret+port[t-count_long]-port[t]
		
		if(ret_risk<risk){
			ret=ret_risk
			return(list(ret,trades,sd(port)))
		}
	}
	if(port[T]!=0 && signal==1) ret=ret+port[T-count_short]-port[T]
	if(port[T]!=0 && signal==2) ret=ret+port[T-count_long]-port[T]
	return(list(ret,trades,sd(port[2:length(port)])))
}
