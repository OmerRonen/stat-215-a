#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector similarityRcpp(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  // Calculate the euclidian distance between <x> and <y>.
  
  // C++ requires initialization of variables.
  double result = 0.0;
  

  
  
  // This is the length of the x vector.
  int n = x.size();
  
  double c_12 = n;
  double c_11 = n;
  double c_22 = n;
  
  // Check that the size is the same and return NA if it is not.
  if (y.size() != n) {
    Rcpp::Rcout << "Error: the size of x and y must be the same.\n";
    return(Rcpp::NumericVector::create(NA_REAL));
  }
  
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < i; j++) {
      // Rcout << "The value of x i : " << x[i] << "\n";
      // Rcout << "The value of x j : " << x[j] << "\n";
      // Rcout << "The value of y i : " << y[i] << "\n";
      // Rcout << "The value of y j : " << y[j] << "\n";
    double x_sim = x[i]==x[j];
    // Rcout << "The value of x_sim : " << x_sim << "\n";
    double y_sim = y[j]==y[i];
    // Rcout << "The value of y_sim : " << y_sim << "\n";
    double sim = x_sim * y_sim;
    // Rcout << "The value of sim : " << sim << "\n";
    
    c_11 += x_sim*2;
    c_22 += y_sim*2;
    
    c_12 += sim*2;

    }}
  // Rcout << "The value of c_11 : " << c_11 << "\n";
  // 
  // Rcout << "The value of c_22 : " << c_22 << "\n";
  // Rcout << "The value of c_12 : " << c_12 << "\n";
  
  result = c_12/sqrt(c_22 * c_11);
  // We need to convert between the double type and the R numeric vector type.
  return Rcpp::NumericVector::create(result);
}