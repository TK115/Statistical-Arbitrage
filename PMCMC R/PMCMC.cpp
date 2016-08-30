//in R
//install.packages("Rcpp")
//library(Rcpp)

//p=PF_rcpp(y=y,rho=rho,tau=tau,sigma=sigma,df=df,N=N,T=T)[[3]] or
//p=PF_rcpp(y=y,rho=rho,tau=tau,sigma=sigma,df=df,N=N,T=T)

#include <Rcpp.h>
#include <ctime>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
// or List instead of double
double PF_rcpp(NumericVector y,double rho,double tau,double sigma,double df, int N,int T){
  NumericVector xx=NumericVector(Dimension(N,T+1));
  NumericVector w=NumericVector(Dimension(N,T+1));
  NumericVector ww=NumericVector(Dimension(N,T+1));
  NumericVector yy=NumericVector(Dimension(N,T+1));
  
  for(int i=0;i<N;i++){
    for(int j=0;j<T+1;j++){
      yy(i,j)=y[j];
    }
  }
  
  for(int i=0;i<N;i++){
    w(i,0)=1.0/double(N);
    xx(i,0)=R::rt(df);
  }
  
  double sum=0.0;
  for(int i=0;i<N;i++){
    sum+=w(i,0);
  }
  
  for(int i=0;i<N;i++){
    ww(i,0)=w(i,0)/sum;
  }
  
  for(int j=0;j<T;j++){
    double sum_w=0.0;
    //double ess=0.0;
    for(int i=0;i<N;i++){
      xx(i,j+1)=rho*xx(i,j)+tau*R::rt(df);
      
      int a=0;
      w(i,j+1)=1.0/sigma*R::dt((yy(i,j+1)-xx(i,j+1))/sigma,df,a);	
      sum_w+=w(i,j+1);
      //ess+=pow(w(i,j+1),2.0);
    }
    vector<double> p(N);
    for(int i=0;i<N;i++){
      ww(i,j+1)=w(i,j+1)/sum_w;
      p[i]=ww(i,j+1);
    }
      IntegerVector anc(N);
      rmultinom(N, p.data(), N, anc.begin()); // Resample step
      for(int i=0;i<N;i++){
        xx(i,j+1)=anc[i];
    }
  }
  
  double p=1.0;
  vector<double> colmean(T+1);
  for(int j=0;j<T+1;j++){
    for(int i=0;i<N;i++){
      colmean[j]+=1.0/double(N)*w(i,j);
    }
    p*=colmean[j];
  }
  //return Rcpp::List::create(Rcpp::Named("w")=w,Rcpp::Named("xx")=xx,Rcpp::Named("p")=p,Rcpp::Named("colmean")=colmean);
  return p;
}

// [[Rcpp::export]]
double min_rcpp(double x,double y){
  if(x<y) return x;
  else return y;
}


// [[Rcpp::export]]
double pdf_invgamma(double x, double alpha, double beta) {
  if (alpha <= 0.0 || beta <= 0.0) {
    Rcpp::warning("NaNs produced");
    return NAN;
  }
  if (x > 0.0)
    //return (pow(x, -alpha-1.0) * exp(-1.0/(beta*x))) / (R::gammafn(alpha) * pow(beta, alpha));
    return pow(x, -alpha-1.0) * exp(-beta/x) / R::gammafn(alpha) * pow(beta, alpha); //parameterisation as densigamma
  else
    return 0.0;
}

// [[Rcpp::export]]

double norm_rs(double a, double b)
{
  double  x;
  x = Rf_rnorm(0.0, 1.0);
  while( (x < a) || (x > b) ) x = norm_rand();
  return x;
}

// half_norm_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) (with a > 0) using half normal rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double half_norm_rs(double a, double b)
{
  double   x;
  x = fabs(norm_rand());
  while( (x<a) || (x>b) ) x = fabs(norm_rand());
  return x;
}

// [[Rcpp::export]]

double unif_rs(double a, double b)
{
  double xstar, logphixstar, x, logu;
  
  // Find the argmax (b is always >= 0)
  // This works because we want to sample from N(0,1)
  if(a <= 0.0) xstar = 0.0;
  else xstar = a;
  logphixstar = R::dnorm(xstar, 0.0, 1.0, 1.0);
  
  x = R::runif(a, b);
  logu = log(R::runif(0.0, 1.0));
  while( logu > (R::dnorm(x, 0.0, 1.0,1.0) - logphixstar))
  {
    x = R::runif(a, b);
    logu = log(R::runif(0.0, 1.0));
  }
  return x;
}

// exp_rs(a, b)
// generates a sample from a N(0,1) RV restricted to the interval
// (a,b) using exponential rejection sampling.
// ======================================================================

// [[Rcpp::export]]

double exp_rs(double a, double b)
{
  double  z, u, rate;
  
  //  Rprintf("in exp_rs");
  rate = 1/a;
  //1/a
  
  // Generate a proposal on (0, b-a)
  z = R::rexp(rate);
  while(z > (b-a)) z = R::rexp(rate);
  u = R::runif(0.0, 1.0);
  
  while( log(u) > (-0.5*z*z))
  {
    z = R::rexp(rate);
    while(z > (b-a)) z = R::rexp(rate);
    u = R::runif(0.0,1.0);
  }
  return(z+a);
}


// [[Rcpp::export]]
double rnorm_trunc (double mu, double sigma, double lower, double upper)
{
  int change;
  double a, b;
  double logt1 = log(0.150), logt2 = log(2.18), t3 = 0.725;
  double z, tmp, lograt;
  
  change = 0;
  a = (lower - mu)/sigma;
  b = (upper - mu)/sigma;
  
  // First scenario
  if( (a == R_NegInf) || (b == R_PosInf))
  {
    if(a == R_NegInf)
    {
      change = 1;
      a = -b;
      b = R_PosInf;
    }
    
    // The two possibilities for this scenario
    if(a <= 0.45) z = norm_rs(a, b);
    else z = exp_rs(a, b);
    if(change) z = -z;
  }
  // Second scenario
  else if((a * b) <= 0.0)
  {
    // The two possibilities for this scenario
    if((R::dnorm(a, 0.0, 1.0,1.0) <= logt1) || (R::dnorm(b, 0.0, 1.0, 1.0) <= logt1))
    {
      z = norm_rs(a, b);
    }
    else z = unif_rs(a,b);
  }
  // Third scenario
  else
  {
    if(b < 0)
    {
      tmp = b; b = -a; a = -tmp; change = 1;
    }
    
    lograt = R::dnorm(a, 0.0, 1.0, 1.0) - R::dnorm(b, 0.0, 1.0, 1.0);
    if(lograt <= logt2) z = unif_rs(a,b);
    else if((lograt > logt1) && (a < t3)) z = half_norm_rs(a,b);
    else z = exp_rs(a,b);
    if(change) z = -z;
  }
  double output;
  output = sigma*z + mu;
  return (output);
}


// [[Rcpp::export]]
// full model for rho, tau, sigma
List gmhpfull(double rho_0,double tau_0, double sigma_0,double sigma_rho,double sigma_tau,double sigma_sigma,
              int nsteps,NumericVector y,double df, int N,int T){
  std::clock_t start;
  double duration;
  start = std::clock();
  
  vector<double> X(nsteps); vector<double> Z(nsteps); vector<double> V(nsteps); vector<double> lik(nsteps);
  X[0]=rho_0; Z[0]=tau_0; V[0]=sigma_0; //rho, tau, sigma
  lik[0]=PF_rcpp(y,X[0],Z[0],V[0],df,N,T);
  
  for(int i=1;i<nsteps;i++){
    double W= R::rlnorm(log(Z[i-1])-pow(sigma_tau,2.0)/2.0,sigma_tau); //NOT SYMMETRIC: proposal for tau
    double U1 =R::runif(0.0,1.0);
    if (U1<=min_rcpp(PF_rcpp(y,X[i-1],W,V[i-1],df,N,T)*R::dunif(X[i-1],-1.0,1.0,0.0)*pdf_invgamma(W,1.0,1.0)*pdf_invgamma(V[i-1],1.0,1.0)*R::dlnorm(W,log(Z[i-1])-pow(sigma_tau,2.0)/2.0,sigma_tau,0)/(PF_rcpp(y,X[i-1],Z[i-1],V[i-1],df,N,T)*R::dunif(X[i-1],-1.0,1.0,0.0)*pdf_invgamma(Z[i-1],1.0,1.0)*pdf_invgamma(V[i-1],1.0,1.0)*R::dlnorm(Z[i-1],log(W)-pow(sigma_tau,2.0)/2.0,sigma_tau,0)),1)){ //tau
      Z[i]= W;
    }else{
      Z[i] = Z[i-1];
    }
    
    double R= R::rlnorm(log(V[i-1])-pow(sigma_sigma,2.0)/2.0,sigma_sigma);
    //NOT SYMMETRIC: proposal for sigma
    double U3= R::runif(0.0,1.0);
    if (U3<=min_rcpp(PF_rcpp(y,X[i-1],Z[i],R,df,N,T)*R::dunif(X[i-1],-1.0,1.0,0.0)*pdf_invgamma(Z[i],1.0,1.0)*pdf_invgamma(R,1.0,1.0)*R::dlnorm(R,log(V[i-1])-pow(sigma_sigma,2.0)/2.0,sigma_sigma,0)/(PF_rcpp(y,X[i-1],Z[i],V[i-1],df,N,T)*R::dunif(X[i-1],-1.0,1.0,0.0)*pdf_invgamma(Z[i],1.0,1.0)*pdf_invgamma(V[i-1],1.0,1.0)*R::dlnorm(V[i-1],log(R)-pow(sigma_sigma,2.0)/2.0,sigma_sigma,0)),1)){ //sigma
      V[i] = R;
    }else{
      V[i] = V[i-1];
    }	
    
    double Y=rnorm_trunc(X[i-1],sigma_rho,-1.0,1.0); //SYMMETRIC: proposal for rho
    double U2=R::runif(0.0,1.0);
    if (U2<=min_rcpp(PF_rcpp(y,Y,Z[i],V[i],df,N,T)*R::dunif(Y,-1.0,1.0,0.0)*pdf_invgamma(Z[i],1.0,1.0)*pdf_invgamma(V[i],1.0,1.0)/(PF_rcpp(y,X[i-1],Z[i],V[i],df,N,T)*R::dunif(X[i-1],-1.0,1.0,0.0)*pdf_invgamma(Z[i],1.0,1.0)*pdf_invgamma(V[i],1.0,1.0)),1)){ //rho
      X[i]=Y;
    }else{
      X[i]=X[i-1];
    }
    lik[i]=PF_rcpp(y,X[i],Z[i],V[i],df,N,T);
    if(i%20==0){
      duration = ( std::clock() - start ) / (double) CLOCKS_PER_SEC;
      std::cout<<"iteration: "<< i <<'\t'<< "time: "<<duration<<'\n';
    }
  }
  return Rcpp::List::create(Rcpp::Named("X")=X,Rcpp::Named("Z")=Z,Rcpp::Named("V")=V,Rcpp::Named("lik")=lik);
}

