#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec grad(arma::vec X,int U,arma::vec beta
             ,int p
             ,int q
             , IntegerVector S,int r){
  
  int N = X.size();
  arma::vec epsi = arma::zeros(N);
  arma::mat Z = arma::zeros(p+q+r,N);
  arma::vec gradient = arma::zeros(p+q+r);
  
  for (int t=U;t<N;t++){
    arma::vec X_G(p+q+r);
    for (int i=0;i<p;i++){
      X_G[i] = X[t-i-1];
    }
    for (int j=0;j<q;j++){
      X_G[p+j] = epsi[t-j-1];
    }
    for (int k=0;k<r;k++){
      X_G[p+q+k] = X[t-S[k]];
    }
    double ac=0;
    for (int i=0;i<(p+q+r);i++){
      ac += beta[i]*X_G[i];
    }
    epsi[t] = X[t] - ac;
    
    arma::vec acc = arma::zeros(p+q+r);
    for (int j=0;j<q;j++){
      acc += beta[p+j]*Z.col(t-j-1);
    }
    Z.col(t) = - X_G - acc;
    
    gradient += epsi[t]*Z.col(t);
  }

  return gradient;
}





