#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double arma_weighted_mean(const arma::vec& x, const arma::vec& w) {
  return arma::sum(x % w) / arma::sum(w);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec arma_running_mean(const arma::vec& x) {
  
  arma::vec mean_x(x.n_elem);
  arma::running_stat<double> stats;
  for(arma::uword i = 0; i < x.n_elem; i++) {
    stats(x(i));
    mean_x(i) = stats.mean();
  }
  return mean_x;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec arma_running_weighted_mean(const arma::vec& x, const arma::vec& w) {
  
  arma::vec mean_x(x.n_elem);
  double sum_w = w(0);
  mean_x(0) = x(0);
  for(arma::uword i = 1; i < x.n_elem; i++) {
    double temp = w(i) + sum_w;
    double diff = x(i) - mean_x(i - 1);
    double temp2 = diff * w(i) / temp;
    mean_x(i) = mean_x(i - 1) + temp2;
    sum_w = temp;
  }
  return mean_x;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double arma_weighted_var(const arma::vec& x, const arma::vec& w, unsigned int ml) {
  
  double sum_w = 0.0;
  double mean_x = 0.0;
  double s = 0.0;
  
  for(arma::uword i = 0; i < x.n_elem; i++) {
    double temp = w(i) + sum_w;
    double diff = x(i) - mean_x;
    double temp2 = diff * w(i) / temp;
    mean_x += temp2;
    s += sum_w * diff * temp2;
    sum_w = temp;
  }
  s /= sum_w;
  if (ml == 1) {
    s /= (1.0 - arma::accu(arma::square(w)) / (sum_w * sum_w));
  }
  return s;
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec arma_running_var(const arma::vec& x, unsigned int ml) {
  
  arma::vec var_x(x.n_elem);
  arma::running_stat<double> stats;
  for(arma::uword i = 0; i < x.n_elem; i++) {
    stats(x(i));
    var_x(i) = stats.var(!ml);
  }
  return var_x;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec arma_running_weighted_var(const arma::vec& x, const arma::vec& w, unsigned int ml) {
  
  arma::vec sum_w(x.n_elem);
  sum_w(0) = w(0);
  arma::vec var_x(x.n_elem);
  var_x(0) = 0.0;
  double mean_x = x(0);
  
  for(arma::uword i = 1; i < x.n_elem; i++) {
    double temp = w(i) + sum_w(i - 1);
    double diff = x(i) - mean_x;
    double temp2 = diff * w(i) / temp;
    mean_x += temp2;
    var_x(i) = var_x(i - 1) + sum_w(i - 1) * diff * temp2;
    sum_w(i) = temp;
  }
  
  var_x /= sum_w;
  if (ml == 1) {
    var_x = var_x / (1.0 - arma::cumsum(arma::square(w)) / arma::square(sum_w));
  }
  
  return var_x;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
double arma_weighted_se(const arma::vec& x, const arma::vec& w) {
  
  double mean_x = arma_weighted_mean(x, w);
  arma::vec wnorm = w / arma::sum(w);
  return sqrt(arma::accu(arma::square(wnorm % (x - mean_x))));
}


