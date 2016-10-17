#' Compute a weighted variance of a vector
#'
#' Computes a weighted variance of a vector.
#' 
#' @export
#' @param x A numeric vector.
#' @param w A numeric vector of non-negative weights. Will be automatically normalized to sum to one.
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} (and corresponding weights in \code{w}) are
#' omitted from the computation. Default is \code{FALSE}.
#' @param method Estimator type, either \code{"unbiased"} (default) or \code{"moment"}. In unbiased case the 
#' sum of squares is divided by \code{length(x) - 1}, whereas in the latter case the division is by \code{length(x)}.
#' @return A weighted variance.
weighted_var <- function(x, w, na.rm = FALSE, method = c("unbiased", "moment")) {
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  
  if (na.rm) {
    ind <- !is.na(x)
    arma_weighted_var(x[ind], w[ind], method)
  } else {
    arma_weighted_var(x, w, method)
  }
}
#' Compute running variance of a vector
#' 
#' Computes running variance of a vector, returning the values from each step.
#' 
#' @export
#' @inheritParams weighted_var
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} are
#' omitted from the computation. Default is \code{FALSE}.
#' @return A vector containing the recursive variance estimates.
running_var <- function(x, na.rm = FALSE, method = c("unbiased", "moment")) {
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  if (na.rm) {
    ind <- !is.na(x)
    arma_running_var(x[ind], method)
  } else {
    arma_running_var(x, method)
  }
}

#' Compute running weighted variance of a vector
#'
#' Computes running weighted variance of a vector, returning the values from each step.
#' 
#' @export
#' @inheritParams weighted_var
#' @return A vector containing the recursive weighted variance estimates.
running_weighted_var <- function(x, w, na.rm = FALSE, method = c("unbiased", "moment")) {
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  if (na.rm) {
    ind <- !is.na(x)
    arma_running_weighted_var(x[ind], w[ind], method)
  } else {
    arma_running_weighted_var(x, w, method)
  }
}
