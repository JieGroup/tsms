#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
double likeli(arma::vec X,int U,
              arma::vec phi,int p,
              arma::vec psi,int q,
              arma::vec tau, IntegerVector S,int r,
              double sigma){
  int N = X.size();
  double loglikeli = 0;
  arma::vec epsi = arma::zeros(N);
  
  for (int i=U;i<N;i++){
    double X_lag = 0;
    for (int j=0;j<p;j++){
      X_lag = X_lag + phi[j]*X[i-j-1];
    }
    
    double epsi_lag = 0;
    for (int k=0;k<q;k++){
      epsi_lag = epsi_lag + psi[k]*epsi[i-k-1];
    }
    
    double X_sea = 0;
    for (int l=0;l<r;l++){
      X_sea = X_sea + tau[l]*X[i-S[l]];
    }
    
    epsi[i] = X[i] - X_lag - epsi_lag - X_sea;
    loglikeli += log(1/sqrt(2*M_PI*sigma*sigma))+(-(epsi[i]*epsi[i]/(2*sigma*sigma)));
  }
  return loglikeli;
}





