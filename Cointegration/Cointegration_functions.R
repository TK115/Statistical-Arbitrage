create.spread<-function(s1,s2,cor_thresh,p_thresh){
	if(length(s1)>length(s2)) s1=s1[1:length(s2)]
	else s2=s2[1:length(s1)]
	s1=log(s1); s2=log(s2)
	
	# DF-test (k=0) i.e. RW or RW with drift
	if(adf.test(s1,k=0)$p.value<p_thresh || adf.test(s2,k=0)$p.value<p_thresh){
		return("S1 or S2 not I(1)")
	} #NB: same as adfTest with type="ct"
	
	if(abs(cor(diff(s1),diff(s2)))<cor_thresh) return("Corr of log-returns below threshold")
	
	lm=lm(s1~s2)
	u=lm$coefficients[1]
	g=lm$coefficients[2]
	z=lm$residuals #s1-g*s2-u
	
	list=adfTest(z,lags=0,type="nc"); lag=0
	for(i in 1:(12*(length(z)/100)^0.25)){
		temp=adfTest(z,lags=i,type="nc")
		if(temp@test$statistic<list@test$statistic){
			list=temp
			lag=i
		}
	}
	statistic=list@test$statistic
	p.val=list@test$p.value
	return(list(u,g,statistic,"p.value"=p.val,lag,z))
}

###########################################################
create.spread.opt<-function(s1,s2,cor_thresh,p_thresh){
	if(length(s1)>length(s2)) s1=s1[1:length(s2)]
	else s2=s2[1:length(s1)]
	s1=log(s1); s2=log(s2)

	if(adf.test(s1,k=0)$p.value<p_thresh || adf.test(s2,k=0)$p.value<p_thresh){
		return("S1 or S2 not I(1)")}
	if(abs(cor(diff(s1),diff(s2)))<cor_thresh) return("Corr of log-returns below threshold")
	
	lm1=lm(s1~s2); lm2=lm(s2~s1)
	#lm1=rlm(s1~s2,psi=psi.hampel); lm2=rlm(s2~s1,psi=psi.hampel)
	u1=lm1$coefficients[1]; u2=lm2$coefficients[1]
	g1=lm1$coefficients[2]; g2=lm2$coefficients[2]
	z1=lm1$residuals; z2=lm2$residuals
	#z1=s1-g1*s2-u1; z2=s2-g2*s1-u2
	
	lag1=floor(12*(length(z1)/100)^0.25)
	lag2=floor(12*(length(z2)/100)^0.25)

	while(abs(adfTest(z1,lags=lag1,type="nc")@test$statistic)<1.6 && lag1>0) lag1=lag1-1
	while(abs(adfTest(z2,lags=lag2,type="nc")@test$statistic)<1.6 && lag2>0) lag2=lag2-1
	
	list1=adfTest(z1,lags=lag1,type="nc")
	list2=adfTest(z2,lags=lag2,type="nc")

	statistic1=list1@test$statistic; statistic2=list2@test$statistic
	p.val1=list1@test$p.val; p.val2=list2@test$p.val
	
	if(statistic1<statistic2 && p.val1<=0.05 && kpss.test(z1)$p.value>=0.05){
		#cat("s1=g*s2+u\n")
		return(list(c(u1,g1,statistic1),"z1"=z1))
	}else if(statistic2<statistic1 && p.val2<=0.05 && kpss.test(z2)$p.value>=0.05){
		#cat("s2=g*s1+u\n")
		return(list(c(u2,g2,statistic2),"z2"=z2))
	}else{
		return("No Cointegration between S1 and S2")
	}
}