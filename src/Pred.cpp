#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec Pred(arma::vec X,int U,int predict_t,
                  arma::vec phi,int p,
                  arma::vec psi,int q,
                  arma::vec tau, IntegerVector S,int r){
  
  int N = X.size();
  arma::vec mid_X = arma::zeros(N+predict_t);
  arma::vec predict_X = arma::zeros(N+predict_t);
  arma::vec epsi = arma::zeros(N+predict_t);
  
  for (int i=U;i<N;i++){
    mid_X[i] = X[i];
  }
  
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
    predict_X[i] = X_lag + epsi_lag + X_sea;
  }
  for (int i=N;i<(N+predict_t);i++){
    double X_lag = 0;
    for (int j=0;j<p;j++){
      X_lag = X_lag + phi[j]*mid_X[i-j-1];
    }
    
    double epsi_lag = 0;
    for (int k=0;k<q;k++){
      epsi_lag = epsi_lag + psi[k]*epsi[i-k-1];
    }
    
    double X_sea = 0;
    for (int l=0;l<r;l++){
      X_sea = X_sea + tau[l]*mid_X[i-S[l]];
    }
    predict_X[i] = X_lag + epsi_lag + X_sea;
    mid_X[i] = predict_X[i];
  }
  
  return predict_X;
}





