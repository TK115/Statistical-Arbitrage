%clear all;
%spret = load('spret.mat');
%sp=load('sp.mat');

%sigma=0.1; N=500; df=2; nsteps=10000;
rho=0.8; tau=1; sigma=0.5; T=250; df=2; N=1000; nsteps=5000;

%Calculate y_[0:T] and trajectory x*_[0:T]
x=zeros(T+1,1);
y=zeros(T+1,1);
x(1)=trnd(df);
for n=1:(T+1)
	x(n+1)=rho*x(n)+tau*trnd(df);
	y(n)=x(n)+sigma*trnd(df);
end

%[X,Z,lik1] = gmhp(0.8,1,0.5,1.1,nsteps,y,N,sigma,df); %rho, tau
%[Y,W,lik2] = gmhp(0,0.8,0.1,0.15,nsteps,sp.labpcexport.spread1,N,sigma,df);
[Rho,Tau,Sigma,lik3] = gmhp_full(0.5,0.7,0.8,0.5,0.7,0.9,nsteps,y,N,df); %rho, tau, sigma

% remove burn-in
Rho=Rho(floor(nsteps/10):end); Tau=Tau(floor(nsteps/10):end); Sigma=Sigma(floor(nsteps/10):end);

figure
histogram(Rho) %0.8
xlabel('rho') 
ylabel('count')
figure; 
histogram(Tau) %1
xlabel('tau') 
ylabel('count')
figure; 
histogram(Sigma) %0.5
xlabel('sigma') 
ylabel('count')
figure;
plot(log(lik3(1:300)));

