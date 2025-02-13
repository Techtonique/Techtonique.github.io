
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double simpson_integral_cpp(NumericVector x, NumericVector y) {
  int n = x.size();
  double h = x[1] - x[0]; // Step size
  double integral = y[0] + y[n - 1]; // Initialize integral with endpoints
  
  for (int i = 1; i < n - 1; i+=2) {
      integral += 4 * y[i];
  }
  
  for (int i = 2; i < n - 1; i+=2) {
      integral += 2 * y[i];
  }
  
  integral *= (h / 3); // Multiply by (h/3)
  return integral;
}