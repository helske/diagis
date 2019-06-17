#include "RcppArmadillo.h"
#ifndef SUMMARY_H
#define SUMMARY_H

double arma_weighted_mean(const arma::vec& x, const arma::vec& w);
arma::vec arma_running_mean(const arma::vec& x);
arma::vec arma_running_weighted_mean(const arma::vec& x, const arma::vec& w);
double arma_weighted_var(const arma::vec& x, const arma::vec& w, unsigned int ml);
arma::vec arma_running_var(const arma::vec& x, unsigned int ml);
arma::vec arma_running_weighted_var(const arma::vec& x, const arma::vec& w, unsigned int ml);

arma::vec arma_weighted_mean_vec(const arma::mat& x, const arma::vec& w);
arma::mat arma_running_mean_vec(const arma::mat& x);
arma::mat arma_running_weighted_mean_vec(const arma::mat& x, const arma::vec& w);
arma::mat arma_weighted_var_vec(arma::mat x, const arma::vec& w, unsigned int ml);
arma::mat arma_weighted_var_vec_t(const arma::mat& x, const arma::vec& w, unsigned int ml);
#endif
