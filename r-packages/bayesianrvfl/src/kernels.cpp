#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double l2_norm(NumericVector x)
{
  unsigned long int n = x.size();
  double res = 0;
  for(int i = 0; i < n; i++) {
    res += pow(x(i), 2);
  }
  return(sqrt(res));
}

// [[Rcpp::export]]
NumericMatrix matern52_kxx_cpp(NumericMatrix x, double sigma, double l)
{
  unsigned long int n = x.nrow();
  NumericMatrix res(n, n);
  NumericVector r(n);
  double sqrt5 = sqrt(5);
  double temp = 0;

  for(int i = 0; i < n; i++) {
    for(int j = i; j < n; j++) {
      r = x(i, _) - x(j, _);
      temp = sqrt5*l2_norm(r)/sigma;
      res(i , j) = (1 + temp + pow(temp, 2)/3)*exp(-temp);
      res(j , i) = res(i , j);
    }
  }
  return(pow(l, 2)*res);
}

// [[Rcpp::export]]
NumericVector matern52_kxy_cpp(NumericMatrix x, NumericVector y, double sigma, double l)
{
  unsigned long int m = x.ncol();
  unsigned long int n = y.size();
  if (m != n) {
    ::Rf_error("you must have x.col() == y.size()");
  }
  unsigned long int k = x.nrow();

  NumericVector res(k);
  NumericVector r(k);
  double temp;
  double sqrt5 = sqrt(5);

  for(int i = 0; i < k; i++) {
    r = x(i, _) - y;
    temp = sqrt5*l2_norm(r)/sigma;
    res(i) = (1 + temp + pow(temp, 2)/3)*exp(-temp);
  }
  return(pow(l, 2)*res);
}
