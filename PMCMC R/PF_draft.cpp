#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


//install.packages("Rcpp")
//library(Rcpp)

// [[Rcpp::export]]

List PF_rcpp(NumericVector y,
             double rho,
             double tau,
             double sigma,
             double df,
             int N,
             int T){
  
  
  NumericVector xx=NumericVector(Dimension(N,T+1));
  NumericVector w=NumericVector(Dimension(N,T+1));
  NumericVector ww=NumericVector(Dimension(N,T+1));
  NumericVector xx_res=NumericVector(Dimension(N,T+1));
  NumericVector yy=NumericVector(Dimension(N,T+1));
  
  for(int i=0;i<N;i++){
    for(int j=0;j<T+1;j++){
      yy(i,j)=y[j];
    }
  }
  
  for(int i=0;i<N;i++){
    w(i,0)=1.0/double(N);
    xx(i,0)=R::rt(df);
    xx_res(i,0)=xx(i,0);
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
    double ess=0.0;
    for(int i=0;i<N;i++){
      
      xx(i,j+1)=rho*xx_res(i,j)+tau*R::rt(df);
      int a=0;
      w(i,j+1)=1.0/sigma*R::dt((yy(i,j+1)-xx(i,j+1))/sigma,df,a);	
      sum_w+=w(i,j+1);
      ess+=pow(w(i,j+1),2.0);
    }
    vector<double> p(N);
    vector<double> pp(N);
    for(int i=0;i<N;i++){
      ww(i,j+1)=w(i,j+1)/sum_w;
      p[i]=ww(i,j+1);
      pp[i]=xx(i,j+1);
    }
    //adaptive resampling
    //if(1.0/ess>N/3.0){
    //for(int i=0;i<N;i++){
    //xx_res(i,j+1)=xx(i,j+1);
    //}
    //}else{
    IntegerVector anc(N);
    rmultinom(N, p.data(), N, anc.begin()); // Resample step
    for(int i=0;i<N;i++){
      xx_res(i,j+1)=anc[i];
      //xx_res(i,j+1)=R::sample(pp,1,TRUE,pp);
      //}
    }
  }
  
  //w=PF_rcpp(y=y,rho=rho,tau=tau,sigma=sigma,df=df,N=N,T=T)[[2]]
  //p=exp(sum(log(colMeans(w)))) do in R
  
  return Rcpp::List::create(Rcpp::Named("ww")=ww,
                            Rcpp::Named("w")=w,
                            Rcpp::Named("xx")=xx);
}
