% # calculating p with the Bootstrap Particle Filter (SIR) t dist innovations
function lik = p(N,T,y,rho,tau,sigma,df)
	yy=y(:,ones(N,1))';
	xx=zeros(N,T+1);
	w=zeros(N,T+1);
	ww=zeros(N,T+1);
	xx_res=zeros(N,T+1);
	
	xx(:,1)=trnd(df,N,1);
	w(:,1)=1/N;
	ww(:,1)=w(:,1)/sum(w(:,1));
	xx_res(:,1)=xx(:,1);
    
    for i=1:T
        xx(:,i+1)=rho*xx_res(:,i)+tau*trnd(df,N,1);
		w(:,i+1)=1/sigma*tpdf((yy(:,i+1)-xx(:,i+1))/sigma,df);
		ww(:,i+1)=w(:,i+1)/sum(w(:,i+1));
        %adaptive resampling
        if 1/sum(ww(:,i+1).^2)>N/3 
            xx_res(:,i+1)=xx(:,i+1);
        else 
            xx_res(:,i+1)=randsample(xx(:,i+1),N,true,ww(:,i+1));
        end
    end
    %lik=prod(mean(w));
    lik=exp(sum(log(mean(w))));
end