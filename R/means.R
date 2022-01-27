#' Weighted mean
#'
#' Computes a weighted mean of a vector, matrix, or a three dimensional array.
#' 
#' @export
#' @param x A numeric vector, matrix, three dimensional array, or an \code{mcmc} object from
#' the \code{coda} package. For matrix, the mean is computed for each column, and 
#' for array the sweep is done over the third dimension.
#' @param w A numeric vector of non-negative weights. Will be automatically normalised to sum to one.
#' @param na.rm If \code{TRUE}, \code{NA} values in \code{x} (and corresponding weights in \code{w}) are
#' omitted from the computation. Default is \code{FALSE}. Only used in vector methods.
#' @return A weighted mean.
weighted_mean <- function(x, w, na.rm) {
  if (!(typeof(w) %in% c("integer", "double"))) {
    stop("Argument 'w' must be of type 'integer' or 'double'. ")
  }
  
  UseMethod("weighted_mean", x)
}
#' @export
#' @method weighted_mean ts
weighted_mean.ts <- function(x, w, na.rm = FALSE) {
  weighted_mean(x = unclass(x), w = w, na.rm = na.rm)
}
#' @export
#' @method weighted_mean mcmc
weighted_mean.mcmc <- function(x, w, na.rm = FALSE) {
  dimx <- dim(x)
  if (is.null(dimx)) {
    weighted_mean.numeric(x, w, na.rm)
  } else {
    if (length(dimx) == 2) {
      weighted_mean.matrix(x, w, na.rm)
    } else {
      weighted_mean.array(x, w, na.rm)
    }
  }
}

#' @export
#' @method weighted_mean numeric
weighted_mean.numeric <- function(x, w, na.rm = FALSE) {
  
  if (length(x) != length(w)) stop("'x' and 'w' have unequal lengths. ")
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }
  start <- which(w > 0)[1]
  
  if (na.rm) {
    ind <- which(!is.na(x[start:length(w)]))
    if(length(ind) == 0) return(NA)
    arma_weighted_mean(x[start:length(w)][ind], w[start:length(w)][ind])
  } else {
    arma_weighted_mean(x[start:length(w)], w[start:length(w)])
  }
}
#' @export
#' @method weighted_mean matrix
weighted_mean.matrix<- function(x, w, na.rm = FALSE) {
  
  if (nrow(x) != length(w)) stop("Length of 'w' is not equal to the number of rows in 'x'. ")
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }
  start <- which(w > 0)[1]

  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
  } 
  arma_weighted_mean_vec(x[start:length(w), , drop = FALSE], w[start:length(w)])
}
#' @export
#' @method weighted_mean array
weighted_mean.array<- function(x, w, na.rm = FALSE) {
  
  if (length(dim(x)) != 3) stop("'x' must be three dimensional. ")
  if (dim(x)[3] != length(w)) stop("Length of 'w' is not equal to the third dimension of 'x'. ")
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }  
  start <- which(w > 0)[1]

  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
  }
  arma_weighted_mean_mat(x[,,start:length(w), drop = FALSE], w[start:length(w)])
  
}
#' Running mean
#' 
#' Computes running mean of a vector or matrix, returning the values from each step.
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
#' @method running_mean ts
running_mean.ts <- function(x, na.rm = FALSE) {
  running_mean(x = unclass(x), na.rm = na.rm)
}
#' @export
#' @method running_mean mcmc
running_mean.mcmc <- function(x, na.rm = FALSE) {
  dimx <- dim(x)
  if (is.null(dimx)) {
    running_mean(x, na.rm)
  } else {
    if (length(dimx) == 2) {
      running_mean.matrix(x, na.rm)
    } else {
      running_mean(x, na.rm)
    }
  }
}
#' @export
#' @method running_mean numeric
running_mean.numeric <- function(x, na.rm = FALSE) {
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (na.rm) {
    ind <- which(!is.na(x))
    if(length(ind) == 0) return(NA)
    arma_running_mean(x[ind])
  } else {
    arma_running_mean(x)
  }
}
#' @export
#' @method running_mean matrix
running_mean.matrix <- function(x, na.rm = FALSE) {
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
  }
  arma_running_mean_vec(x)
}
#' Running weighted mean
#'
#' Computes running weighted mean of a vector or matrix, returning the values from each step.
#' 
#' @export
#' @inheritParams weighted_mean
#' @return A vector containing the recursive weighted mean estimates.
running_weighted_mean <- function(x, w, na.rm) {
  if (!(typeof(w) %in% c("integer", "double"))) {
    stop("Argument 'w' must be of type 'integer' or 'double'. ")
  }
  UseMethod("running_weighted_mean", x)
}
#' @export
#' @method running_weighted_mean ts
running_weighted_mean.ts <- function(x, w, na.rm = FALSE) {
  running_weighted_mean(x = unclass(x), w = w, na.rm = na.rm)
}
#' @export
#' @method running_weighted_mean mcmc
running_weighted_mean.mcmc <- function(x, w, na.rm = FALSE) {
  dimx <- dim(x)
  if (is.null(dimx)) {
    running_weighted_mean.numeric(x, w, na.rm)
  } else {
    if (length(dimx) == 2) {
      running_weighted_mean.matrix(x, w, na.rm)
    } else {
      running_weighted_mean(x, w, na.rm)
    }
  }
}
#' @export
#' @method running_weighted_mean numeric
running_weighted_mean.numeric <- function(x, w, na.rm = FALSE) {
  
  if (length(x) != length(w)) stop("'x' and 'w' have unequal lengths. ")
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }
  start <- which(w > 0)[1]

  if (na.rm) {
    ind <- which(!is.na(x[start:length(w)]))
    if(length(ind) == 0) return(NA)
    arma_running_weighted_mean(x[start:length(w)][ind], w[start:length(w)][ind])
  } else {
    arma_running_weighted_mean(x[start:length(w)], w[start:length(w)])
  }
}
#' @export
#' @method running_weighted_mean matrix
running_weighted_mean.matrix <- function(x, w, na.rm = FALSE) {
  
  if (nrow(x) != length(w)) stop("Length of 'w' is not equal to the number of rows in 'x'. ")
  if (length(na.rm) > 1 || !is.logical(na.rm)) stop("Argument 'na.rm' should be a logical of length one.")
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }
  
  start <- which(w > 0)[1]
  if (na.rm) {
    warning("Argument 'na.rm' ignored. ")
  } 
  arma_running_weighted_mean_vec(x[start:length(w), , drop = FALSE], w[start:length(w)])
}
