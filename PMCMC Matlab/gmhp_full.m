function [X,Z,V,lik] = gmhp_full(rho_0,tau_0,sigma_0,sigma_rho,sigma_tau,sigma_sigma,nsteps,y,N,df)
	X=zeros(nsteps,1); Z=zeros(nsteps,1); V=zeros(nsteps,1); lik=zeros(nsteps,1);
	X(1)=rho_0; Z(1)=tau_0; V(1)=sigma_0; %rho, tau, sigma
    lik(1)=p(N,length(y)-1,y,X(1),Z(1),V(1),df);
    tic
    for i=2:nsteps
    	W = lognrnd(log(Z(i-1))-sigma_tau^2/2,sigma_tau); % NOT SYMMETRIC: proposal for tau
    	U1 =rand;
        if U1<=min(p(N,length(y)-1,y,X(i-1),W,V(i-1),df)*unifpdf(X(i-1),-1,1)*inversegampdf(W,1,1)*inversegampdf(V(i-1),1,1)*lognpdf(W,log(Z(i-1))-sigma_tau^2/2,sigma_tau)/(p(N,length(y)-1,y,X(i-1),Z(i-1),V(i-1),df)*unifpdf(X(i-1),-1,1)*inversegampdf(Z(i-1),1,1)*inversegampdf(V(i-1),1,1)*lognpdf(Z(i-1),log(W)-sigma_tau^2/2,sigma_tau)),1); %tau
        	Z(i) = W;
        else
        	Z(i) = Z(i-1);
        end
        
        R = lognrnd(log(V(i-1))-sigma_sigma^2/2,sigma_sigma); % NOT SYMMETRIC: proposal for sigma
    	U3 =rand;
        if U3<=min(p(N,length(y)-1,y,X(i-1),Z(i),R,df)*unifpdf(X(i-1),-1,1)*inversegampdf(Z(i),1,1)*inversegampdf(R,1,1)*lognpdf(R,log(V(i-1))-sigma_sigma^2/2,sigma_sigma)/(p(N,length(y)-1,y,X(i-1),Z(i),V(i-1),df)*unifpdf(X(i-1),-1,1)*inversegampdf(Z(i),1,1)*inversegampdf(V(i-1),1,1)*lognpdf(V(i-1),log(R)-sigma_sigma^2/2,sigma_sigma)),1); %sigma
        	V(i) = R;
        else
        	V(i) = V(i-1);
        end
    		
    	Y = truncnormrnd(1,X(i-1),sigma_rho,-1,1); %SYMMETRIC: proposal for rho
    	U2=rand;
        if U2<=min(p(N,length(y)-1,y,Y,Z(i),V(i),df)*unifpdf(Y,-1,1)*inversegampdf(Z(i),1,1)*inversegampdf(V(i),1,1)/(p(N,length(y)-1,y,X(i-1),Z(i),V(i),df)*unifpdf(X(i-1),-1,1)*inversegampdf(Z(i),1,1)*inversegampdf(V(i),1,1)),1); %rho
    	 	X(i) = Y;
        else
        	X(i) = X(i-1);
        end
        lik(i)=p(N,length(y)-1,y,X(i),Z(i),V(i),df);
        if mod(i,500)==0 
            fprintf('Just finished iteration #%d\n',i); toc
        end
    end
end