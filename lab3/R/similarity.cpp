#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::NumericVector similarityRcpp(Rcpp::NumericVector l, Rcpp::NumericVector y) {
  // similarity implementation in O(k_1*k_2*n)
  double result = 0.0;
  // we clone becasue we later sort
  NumericVector x;
  x=clone(l);
  
  int n = x.size();
  // the range of clusters
  int u_c = l.sort()[n-1];
  int l_c = l.sort()[0];
  
  double c_12 = 0.0;
  double c_11 = 0.0;
  double c_22 = 0.0;
  // we iterate over clusters
  // for each combination j,i we count the number
  // indices for which x=j and y=i
  // then we take the square of that number 
  // so all pairs are counted
  for (int i = l_c; i < u_c+1; i++) {
    for (int j = l_c; j < u_c+1; j++) {

      double n_xy = 0.0;
      double n_yy = 0.0;
      double n_xx = 0.0;

      for (int k = 0; k < n; k++) {

      double x_sim = x[k]==i;

      double y_sim = y[k]==j;

      double sim = x_sim * y_sim;

      n_xy += sim;
      // we don't want to count more than once
      if (j==i){
      n_yy += y_sim;
      n_xx += x_sim;
      }

    }
      c_12 += n_xy*n_xy;

      c_11 += n_xx*n_xx;

      c_22 += n_yy*n_yy;

    }}

  result = c_12/sqrt(c_22 * c_11);
  return Rcpp::NumericVector::create(result);
  }

// [[Rcpp::export]]
Rcpp::NumericVector similarityRcppSlow(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  // similarity implementation in O(n^2)
  
  double result = 0.0;
  int n = x.size();
  // diagonal part
  double c_12 = n;
  double c_11 = n;
  double c_22 = n;

  // going over the two vectors and counting 
  // same clusters
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < i; j++) {

    double x_sim = x[i]==x[j];
    double y_sim = y[j]==y[i];
    double sim = x_sim * y_sim;
    // c matrix is symmetric
    c_11 += x_sim*2;
    c_22 += y_sim*2;

    c_12 += sim*2;

    }}
  result = c_12/sqrt(c_22 * c_11);
  return Rcpp::NumericVector::create(result);
}


