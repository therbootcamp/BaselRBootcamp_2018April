#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector col_means_cpp(DataFrame& df) {
  int n_rows = df.nrows(), n_cols= df.size() ;
  NumericVector means(n_cols);
  for(int j = 0; j < n_cols; ++j) {
    NumericVector column = df[j];
    double sum = 0;
    for(int i = 0; i < n_rows; ++i){
      sum += column[i];
      }
    means[j] = sum / n_rows;  
    }

  return means;
  }