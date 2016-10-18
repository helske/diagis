#' Compute a weighted mean of a vector
#'
#' Computes a weighted mean of a vector.
#' 
#' @export
#' @param x A numeric vector.
#' @param w A numeric vector of non-negative weights. Will be automatically normalized to sum to one.
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} (and corresponding weights in \code{w}) are
#' omitted from the computation. Default is \code{FALSE}.
#' @return A weighted mean.
weighted_mean <- function(x, w, na.rm) {
  UseMethod("weighted_mean", x)
}
#' @export
#' @method weighted_mean numeric
weighted_mean.numeric <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    ind <- !is.na(x)
    arma_weighted_mean(x[ind], w[ind])
  } else {
    arma_weighted_mean(x, w)
  }
}
#' @export
#' @method weighted_mean matrix
weighted_mean.matrix<- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_weighted_mean_vec(x, w)
  } else {
    arma_weighted_mean_vec(x, w)
  }
}
#' @export
#' @method weighted_mean array
weighted_mean.array<- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_weighted_mean_mat(x, w)
  } else {
    arma_weighted_mean_mat(x, w)
  }
}
#' Compute running mean of a vector
#' 
#' Computes running mean of a vector, returning the values from each step.
#'
#' @export
#' @inheritParams weighted_mean
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} are
#' omitted from the computation. Default is \code{FALSE}.
#' @return A vector containing the recursive mean estimates.
running_mean <- function(x, na.rm) {
  UseMethod("running_mean", x)
}
#' @export
#' @method running_mean numeric
running_mean.numeric <- function(x, na.rm = FALSE) {
  if (na.rm) {
    ind <- !is.na(x)
    arma_running_mean(x[ind])
  } else {
    arma_running_mean(x)
  }
}
#' @export
#' @method running_mean matrix
running_mean.matrix <- function(x, na.rm = FALSE) {
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_running_mean_vec(x)
  } else {
    arma_running_mean_vec(x)
  }
}
#' Compute running weighted mean of a vector
#'
#' Computes running weighted mean of a vector, returning the values from each step.
#' 
#' @export
#' @inheritParams weighted_mean
#' @return A vector containing the recursive weighted mean estimates.
running_weighted_mean <- function(x, w, na.rm) {
  UseMethod("running_weighted_mean", x)
}
#' @export
#' @method running_weighted_mean numeric
running_weighted_mean.numeric <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    ind <- !is.na(x)
    arma_running_weighted_mean(x[ind], w[ind])
  } else {
    arma_running_weighted_mean(x, w)
  }
}
#' @export
#' @method running_weighted_mean matrix
running_weighted_mean.matrix <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_running_weighted_mean(x, w)
  } else {
    arma_running_weighted_mean(x, w)
  }
}
