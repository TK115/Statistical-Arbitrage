function [X,Z,lik] = gmhp(rho_0,tau_0,sigma_rho,sigma_tau,nsteps,y,N,sigma,df)
	X=zeros(nsteps,1); Z=zeros(nsteps,1); lik=zeros(nsteps,1);
	X(1)=rho_0; Z(1)=tau_0; %rho, tau
    lik(1)=p(N,length(y)-1,y,X(1),Z(1),sigma,df);
    for i=2:nsteps
    	W = lognrnd(log(Z(i-1))-sigma_tau^2/2,sigma_tau); % NOT SYMMETRIC: proposal for tau
    	U1 =rand;
        if U1<=min(p(N,length(y)-1,y,X(i-1),W,sigma,df)*unifpdf(X(i-1),-1,1)*inversegampdf(W,1,1)*lognpdf(W,log(Z(i-1))-sigma_tau^2/2,sigma_tau)/(p(N,length(y)-1,y,X(i-1),Z(i-1),sigma,df)*unifpdf(X(i-1),-1,1)*inversegampdf(Z(i-1),1,1)*lognpdf(Z(i-1),log(W)-sigma_tau^2/2,sigma_tau)),1); %tau
        	Z(i) = W;
        else
        	Z(i) = Z(i-1);
        end
    		
    	Y = truncnormrnd(1,X(i-1),sigma_rho,-1,1); % SYMMETRIC: proposal for rho
    	U2=rand;
        if U2<=min(p(N,length(y)-1,y,Y,Z(i),sigma,df)*unifpdf(Y,-1,1)*inversegampdf(Z(i),1,1)/(p(N,length(y)-1,y,X(i-1),Z(i),sigma,df)*unifpdf(X(i-1),-1,1)*inversegampdf(Z(i),1,1)),1); %rho
    	 	X(i) = Y;
        else
        	X(i) = X(i-1);
        end
        lik(i)=p(N,length(y)-1,y,X(i),Z(i),sigma,df);
    end
end