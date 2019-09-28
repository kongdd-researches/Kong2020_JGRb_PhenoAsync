// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// sum_perc
double sum_perc(NumericVector x, double perc);
RcppExport SEXP _PhenoAsync_sum_perc(SEXP xSEXP, SEXP percSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type perc(percSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_perc(x, perc));
    return rcpp_result_gen;
END_RCPP
}
// mean_perc
double mean_perc(NumericVector x, double perc);
RcppExport SEXP _PhenoAsync_mean_perc(SEXP xSEXP, SEXP percSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type perc(percSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_perc(x, perc));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_PhenoAsync_sum_perc", (DL_FUNC) &_PhenoAsync_sum_perc, 2},
    {"_PhenoAsync_mean_perc", (DL_FUNC) &_PhenoAsync_mean_perc, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_PhenoAsync(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
