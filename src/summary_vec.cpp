#include "RcppArmadillo.h"
#include "summary.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec arma_weighted_mean_vec(const arma::mat& x, const arma::vec& w) {
  arma::vec mean_x(x.n_cols);
  for (arma::uword i = 0; i < x.n_cols; i++) {
    mean_x(i) = arma_weighted_mean(x.col(i), w);
  }
  return mean_x;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat arma_running_mean_vec(const arma::mat& x) {
  arma::mat mean_x(x.n_rows, x.n_cols);
  for (arma::uword i = 0; i < x.n_cols; i++) {
    mean_x.col(i) = arma_running_mean(x.col(i));
  }
  return mean_x;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat arma_running_weighted_mean_vec(const arma::mat& x, const arma::vec& w) {
  arma::mat mean_x(x.n_rows, x.n_cols);
  for (arma::uword i = 0; i < x.n_cols; i++) {
    mean_x.col(i) = arma_running_weighted_mean(x.col(i), w);
  }
  return mean_x;
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat arma_weighted_var_vec(arma::mat x, const arma::vec& w, unsigned int ml) {
  arma::mat var_x(x.n_cols, x.n_cols, arma::fill::zeros);
  arma::vec mean_x(x.n_cols, arma::fill::zeros);
  double sum_w = 0.0;
  arma::inplace_trans(x);
  for(arma::uword i = 0; i < x.n_cols; i++) {
      double tmp = w(i) + sum_w;
      arma::vec diff = x.col(i) - mean_x;
      mean_x += diff * w(i) / tmp;
      var_x +=  w(i) * diff * (x.col(i) - mean_x).t();
      sum_w = tmp;
    }
  var_x /= sum_w;
  if (ml == 1) {
    var_x /= (1.0 - arma::accu(arma::square(w)) / (sum_w * sum_w));
  }
  return var_x;
}


// x is already transposed, otherwise just like above
// used internally by arma_weighted_var_mat
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat arma_weighted_var_vec_t(const arma::mat& x, const arma::vec& w, unsigned int ml) {
  arma::mat var_x(x.n_rows, x.n_rows, arma::fill::zeros);
  arma::vec mean_x(x.n_rows, arma::fill::zeros);
  double sum_w = 0.0;
  for(arma::uword i = 0; i < x.n_cols; i++) {
      double tmp = w(i) + sum_w;
      arma::vec diff = x.col(i) - mean_x;
      mean_x += diff * w(i) / tmp;
      var_x +=  w(i) * diff * (x.col(i) - mean_x).t();
      sum_w = tmp;
    }
  var_x /= sum_w;
  if (ml == 1) {
    var_x /= (1.0 - arma::accu(arma::square(w)) / (sum_w * sum_w));
  }
  return var_x;
}

// don't see interest in running statistics for vectors... 
// make a PR or file an issue in github if you really need it


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec arma_weighted_se_vec(const arma::mat& x, const arma::vec& w) {
  
  arma::vec mean_x = arma_weighted_mean_vec(x, w);
  arma::vec wnorm = w / arma::sum(w);
  arma::vec ses(x.n_cols);
  for (arma::uword i = 0; i < x.n_cols; i++) {
    ses(i) = sqrt(arma::sum(arma::square(wnorm % (x.col(i) - mean_x(i)))));
  }
  return ses;
}

