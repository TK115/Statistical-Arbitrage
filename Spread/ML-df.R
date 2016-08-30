# SPX
mu_sp=mean(index.ret2); sd_sp=sd(index.ret2)
loglik_sp4=0; loglik_sp5=0; loglik_sp6=0
loglik_sp7=0; loglik_sp8=0; loglik_spnorm=0
for(i in 1:length(index.ret2)){
	loglik_spnorm=loglik_spnorm+log(dnorm(index.ret2[i],mean=mu_sp,sd=sd_sp))
	loglik_sp4=loglik_sp4+log(1/sd_sp*dt((index.ret2[i]-mu_sp)/sd_sp,df=4))
	loglik_sp5=loglik_sp5+log(1/sd_sp*dt((index.ret2[i]-mu_sp)/sd_sp,df=5))
	loglik_sp6=loglik_sp6+log(1/sd_sp*dt((index.ret2[i]-mu_sp)/sd_sp,df=6))
	loglik_sp7=loglik_sp7+log(1/sd_sp*dt((index.ret2[i]-mu_sp)/sd_sp,df=7))
	loglik_sp8=loglik_sp8+log(1/sd_sp*dt((index.ret2[i]-mu_sp)/sd_sp,df=8))
}
loglik_spnorm
loglik_sp4
loglik_sp5
loglik_sp6
loglik_sp7
loglik_sp8

#maximum log-likelihood for 7 df

# UKX
mu_uk=mean(index.ret2); sd_uk=sd(index.ret2)
loglik_uk4=0; loglik_uk5=0; loglik_uk6=0
loglik_uk7=0; loglik_uk8=0; loglik_uknorm=0
for(i in 1:length(index.ret2)){
	loglik_uknorm=loglik_uknorm+log(dnorm(index.ret2[i],mean=mu_uk,sd=sd_uk))
	loglik_sp4=loglik_sp4+log(1/sd_uk*dt((index.ret2[i]-mu_uk)/sd_uk,df=4))
	loglik_sp5=loglik_sp5+log(1/sd_uk*dt((index.ret2[i]-mu_uk)/sd_uk,df=5))
	loglik_sp6=loglik_sp6+log(1/sd_uk*dt((index.ret2[i]-mu_uk)/sd_uk,df=6))
	loglik_sp7=loglik_sp7+log(1/sd_uk*dt((index.ret2[i]-mu_uk)/sd_uk,df=7))
	loglik_sp8=loglik_sp8+log(1/sd_uk*dt((index.ret2[i]-mu_uk)/sd_uk,df=8))
}
loglik_uknorm
loglik_sp4
loglik_sp5
loglik_sp6
loglik_sp7
loglik_sp8

#maximum log-likelihood for 7 df