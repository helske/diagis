#' Weighted covariance
#'
#' Computes a weighted variance/covariance of a vector, matrix or a three dimensional array.
#' 
#' @note Compared to some other R functions, here the weights are regarded as probability weights,
#' not frequency weights.
#' 
#' @export
#' @param x A numeric vector, matrix or three dimensional array.
#' For matrix, covariances are computed between columns. 
#' For array, marginal covariances are computed for each column, 
#' i.e. for $m x n x k$ array function returns $m x m x n$ array. 
#' @param w A numeric vector of non-negative weights. Will be automatically normalised to sum to one.
#' @param method Estimator type, either \code{"unbiased"} (default) or \code{"moment"}.
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} (and corresponding weights in \code{w}) are
#' omitted from the computation. Default is \code{FALSE}.
#' @return A weighted variance.
weighted_var <- function(x, w, method, na.rm) {
  if (!(typeof(w) %in% c("integer", "double"))) {
    stop("Argument 'w' must be of type 'integer' or 'double'. ")
  }
  UseMethod("weighted_var", x)
}
#' @export
#' @method weighted_var numeric
weighted_var.numeric <- function(x, w, method = c("unbiased", "moment"), na.rm = FALSE) {
  
  if (length(x) != length(w)) stop("'x' and 'w' have unequal lengths. ")
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  
  if (na.rm) {
    ind <- !is.na(x)
    arma_weighted_var(x[ind], w[ind], method)
  } else {
    arma_weighted_var(x, w, method)
  }
}
#' @export
#' @method weighted_var matrix
weighted_var.matrix <- function(x, w, method = c("unbiased", "moment"), na.rm = FALSE) {
  
  
  if (nrow(x) != length(w)) stop("Length of 'w' is not equal to the number of rows in 'x'. ")
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_weighted_var_vec(x, w, method)
  } else {
    arma_weighted_var_vec(x, w, method)
  }
}
#' @export
#' @method weighted_var array
weighted_var.array <- function(x, w, method = c("unbiased", "moment"), na.rm = FALSE) {
  
  if (length(dim(x)) != 3) stop("'x' must be three dimensional. ")
  if (dim(x)[3] != length(w)) stop("Length of 'w' is not equal to the third dimension of 'x'. ")
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_weighted_var_mat(x, w, method)
  } else {
    arma_weighted_var_mat(x, w, method)
  }
}
#' Running variance of a vector
#' 
#' Computes running variance of a vector, returning the values from each step.
#' 
#' @export
#' @param x A numeric vector
#' @inheritParams weighted_var
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} are
#' omitted from the computation. Default is \code{FALSE}.
#' @return A vector containing the recursive variance estimates.
running_var <- function(x, method = c("unbiased", "moment"), na.rm = FALSE) {
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  if (na.rm) {
    ind <- !is.na(x)
    arma_running_var(x[ind], method)
  } else {
    arma_running_var(x, method)
  }
}

#' Running weighted variance of a vector
#'
#' Computes running weighted variance of a vector, returning the values from each step.
#' 
#' @export
#' @param x A numeric vector, 
#' @inheritParams weighted_var
#' @return A vector containing the recursive weighted variance estimates.
running_weighted_var <- function(x, w, method = c("unbiased", "moment"), na.rm = FALSE) {
  
  if (length(x) != length(w)) stop("'x' and 'w' have unequal lengths. ")
  
  method <- pmatch(match.arg(method), c("unbiased", "moment")) - 1L
  
  if (na.rm) {
    ind <- !is.na(x)
    arma_running_weighted_var(x[ind], w[ind], method)
  } else {
    arma_running_weighted_var(x, w, method)
  }
}
