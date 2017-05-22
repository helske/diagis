#' Weighted standard error
#'
#' Computes a weighted standard error of a vector or matrix.
#' 
#' @note Compared to some other R functions, here the weights are regarded as probability weights,
#' not frequency weights.
#' 
#' @export
#' @param x A numeric vector or matrix.
#' @param w A numeric vector of non-negative weights. Will be automatically normalised to sum to one.
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} (and corresponding weights in \code{w}) are
#' omitted from the computation. Default is \code{FALSE}.
#' @return A weighted variance.
weighted_se <- function(x, w, na.rm) {
  if (!(typeof(w) %in% c("integer", "double"))) {
    stop("Argument 'w' must be of type 'integer' or 'double'. ")
  }
  UseMethod("weighted_se", x)
}
#' @export
#' @method weighted_se ts
weighted_se.ts <- function(x, w, na.rm = FALSE) {
  weighted_se(x = as.numeric(x), w = w, na.rm = na.rm)
}
#' @export
#' @method weighted_se mcmc
weighted_se.mcmc <- function(x, w, na.rm = FALSE) {
  dimx <- dim(x)
  if (is.null(dimx)) {
    weighted_se.numeric(x, w, na.rm)
  } else {
    if (length(dimx) == 2) {
      weighted_se.matrix(x, w, na.rm)
    } else {
      weighted_se(x, w, na.rm)
    }
  }
}
#' @export
#' @method weighted_se numeric
weighted_se.numeric <- function(x, w, na.rm = FALSE) {
  
  if (length(x) != length(w)) stop("'x' and 'w' have unequal lengths. ")
  
  if (na.rm) {
    ind <- !is.na(x)
    arma_weighted_se(x[ind], w[ind])
  } else {
    arma_weighted_se(x, w)
  }
}

#' @export
#' @method weighted_se matrix
weighted_se.matrix <- function(x, w, na.rm = FALSE) {
  
 if (nrow(x) != length(w)) stop("Length of 'w' is not equal to the number of rows in 'x'. ")
  
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
    arma_weighted_se_vec(x, w)
  } else {
    arma_weighted_se_vec(x, w)
  }
}
