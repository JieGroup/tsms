// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <math.h>

using namespace Rcpp;

// Pred
arma::vec Pred(arma::vec X, int U, int predict_t, arma::vec phi, int p, arma::vec psi, int q, arma::vec tau1, IntegerVector S1, int r1, arma::vec tau2, IntegerVector S2, int r2, arma::vec gamma, arma::mat W);
RcppExport SEXP _tsms_Pred(SEXP XSEXP, SEXP USEXP, SEXP predict_tSEXP, SEXP phiSEXP, SEXP pSEXP, SEXP psiSEXP, SEXP qSEXP, SEXP tau1SEXP, SEXP S1SEXP, SEXP r1SEXP, SEXP tau2SEXP, SEXP S2SEXP, SEXP r2SEXP, SEXP gammaSEXP, SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type U(USEXP);
    Rcpp::traits::input_parameter< int >::type predict_t(predict_tSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tau1(tau1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< int >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tau2(tau2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    rcpp_result_gen = Rcpp::wrap(Pred(X, U, predict_t, phi, p, psi, q, tau1, S1, r1, tau2, S2, r2, gamma, W));
    return rcpp_result_gen;
END_RCPP
}
// fit
double fit(arma::vec X, int U, arma::vec beta, int p, int q, IntegerVector S1, int r1, IntegerVector S2, int r2, arma::mat W);
RcppExport SEXP _tsms_fit(SEXP XSEXP, SEXP USEXP, SEXP betaSEXP, SEXP pSEXP, SEXP qSEXP, SEXP S1SEXP, SEXP r1SEXP, SEXP S2SEXP, SEXP r2SEXP, SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type U(USEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< int >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    rcpp_result_gen = Rcpp::wrap(fit(X, U, beta, p, q, S1, r1, S2, r2, W));
    return rcpp_result_gen;
END_RCPP
}
// grad
arma::vec grad(arma::vec X, int U, arma::vec beta, int p, int q, IntegerVector S1, int r1, IntegerVector S2, int r2, arma::mat W);
RcppExport SEXP _tsms_grad(SEXP XSEXP, SEXP USEXP, SEXP betaSEXP, SEXP pSEXP, SEXP qSEXP, SEXP S1SEXP, SEXP r1SEXP, SEXP S2SEXP, SEXP r2SEXP, SEXP WSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type U(USEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< int >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    rcpp_result_gen = Rcpp::wrap(grad(X, U, beta, p, q, S1, r1, S2, r2, W));
    return rcpp_result_gen;
END_RCPP
}
// likeli
double likeli(arma::vec X, int U, arma::vec phi, int p, arma::vec psi, int q, arma::vec tau1, IntegerVector S1, int r1, arma::vec tau2, IntegerVector S2, int r2, arma::vec gamma, arma::mat W, double sigma);
RcppExport SEXP _tsms_likeli(SEXP XSEXP, SEXP USEXP, SEXP phiSEXP, SEXP pSEXP, SEXP psiSEXP, SEXP qSEXP, SEXP tau1SEXP, SEXP S1SEXP, SEXP r1SEXP, SEXP tau2SEXP, SEXP S2SEXP, SEXP r2SEXP, SEXP gammaSEXP, SEXP WSEXP, SEXP sigmaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type U(USEXP);
    Rcpp::traits::input_parameter< arma::vec >::type phi(phiSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type psi(psiSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tau1(tau1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S1(S1SEXP);
    Rcpp::traits::input_parameter< int >::type r1(r1SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type tau2(tau2SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type S2(S2SEXP);
    Rcpp::traits::input_parameter< int >::type r2(r2SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< double >::type sigma(sigmaSEXP);
    rcpp_result_gen = Rcpp::wrap(likeli(X, U, phi, p, psi, q, tau1, S1, r1, tau2, S2, r2, gamma, W, sigma));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tsms_Pred", (DL_FUNC) &_tsms_Pred, 15},
    {"_tsms_fit", (DL_FUNC) &_tsms_fit, 10},
    {"_tsms_grad", (DL_FUNC) &_tsms_grad, 10},
    {"_tsms_likeli", (DL_FUNC) &_tsms_likeli, 15},
    {NULL, NULL, 0}
};

RcppExport void R_init_tsms(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
