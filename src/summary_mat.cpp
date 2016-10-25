#include "RcppArmadillo.h"
#include "summary.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat arma_weighted_mean_mat(const arma::cube& x, const arma::vec& w) {
  arma::mat mean_x(x.n_rows, x.n_cols);
  for (arma::uword i = 0; i < x.n_cols; i++) {
    arma::mat temp(x.tube(arma::span::all, arma::span(i)));
    arma::inplace_trans(temp);
    mean_x.col(i) = arma_weighted_mean_vec(temp, w);
  }
  return mean_x;
}

// computes the marginal covariance matrices x.col(1), x.col(2) etc
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::cube arma_weighted_var_mat(const arma::cube& x, const arma::vec& w, unsigned int ml) {
  
  arma::cube var_x(x.n_cols, x.n_cols, x.n_rows);
  
  for (arma::uword i = 0; i < x.n_rows; i++) {
    var_x.slice(i) = arma_weighted_var_vec_t(x.tube(arma::span(i),arma::span::all), w, ml);
  }
  return var_x;
}

// don't see interest in running statistics for matrices... 
// make a PR or file an issue in github if you really need it
