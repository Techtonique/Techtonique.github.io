#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix cbind_cpp(NumericMatrix x, NumericMatrix y)
{
  unsigned long int x_nrow = x.nrow();
  unsigned long int x_ncol = x.ncol();
  unsigned long int y_nrow = y.nrow();
  unsigned long int y_ncol = y.ncol();
  if (x_nrow != y_nrow) {
    ::Rf_error("you must have x.nrow() == y.nrow()");
  }
  unsigned long int res_ncol = x_ncol + y_ncol;
  NumericMatrix res(x_nrow, res_ncol);

  for(int j = 0; j < res_ncol; j++) {
    if(j < x_ncol)
    {
      res(_ , j) = x(_ , j);
    } else {
      res(_, j) = y(_ , j - x_ncol);
    }
  }

  return res;
}

// [[Rcpp::export]]
double l2_dist(NumericVector x, NumericVector y)
{
  unsigned long int n = x.size();

  if (n != y.size()) {
    ::Rf_error("you must have x.ncol() == y.size()");
  }

  double res = 0;

  for(int i = 0; i < n; i++) {
    res += pow(x(i) - y(i), 2);
  }

  return(sqrt(res));
}

// [[Rcpp::export]]
NumericVector l2_distmat(NumericVector y, NumericMatrix x)
{
  unsigned long int n = x.nrow();
  unsigned long int y_ncol = y.size();

  if (x.ncol() != y_ncol) {
    ::Rf_error("you must have x.ncol() == y.size()");
  }

  NumericVector res(n);

     for(int i = 0; i < n; i++) {
       res(i) = l2_dist(y, x(i, _));
    }

  return(res);
}

// [[Rcpp::export]]
NumericMatrix one_hot_encode_cpp(NumericVector x_clusters, unsigned long int n_clusters)
{

  // if (max(x_clusters) != n_clusters) {
  //   ::Rf_error("max(x_clusters) != n_clusters");
  // }

  unsigned long int n_obs = x_clusters.size();

  // matrix of results
  NumericMatrix res(n_obs, n_clusters);

  for(int i = 0; i < n_obs; i++) {
      res(i, x_clusters(i)-1) = 1;
    }

    return (res);
}


