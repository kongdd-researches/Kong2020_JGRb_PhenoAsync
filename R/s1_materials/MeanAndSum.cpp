#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double sum_cpp(NumericVector x, double perc = 0.8) {
    NumericVector xx(x);
    NumericVector val = na_omit(xx);
    
    if (val.length() < xx.length() * perc){
        return NA_REAL;
    }else{
        return sum(val);
    }
}

// [[Rcpp::export]]
double mean_cpp(NumericVector x, double perc = 0.8) {
    NumericVector xx(x);
    NumericVector val = na_omit(xx);

    if (val.length() < xx.length() * perc){
        return NA_REAL;
    }else{
        return mean(val);
    }
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# timesTwo(42)
*/
