#' Weighted quantiles
#'
#' Computes a weighted quantiles of a vector or matrix. Based on the formula in 
#' Wikipedia (see the vignette) which is one of many ways to compute weighted 
#' quantiles.
#' 
#' @note Compared to some other R functions, here the weights are regarded as 
#' probability weights, not frequency weights.
#' 
#' @export
#' @param x A numeric vector or matrix. For matrix, the quantiles are computed 
#' for each column.
#' @param w A numeric vector of non-negative weights. Will be automatically 
#' normalised to sum to one.
#' @param probs A numeric vector of probabilities with values between 0 and 1.
#' @param na.rm If \code{TRUE}, \code{NA} and \code{NaN} values in \code{x} 
#' (and corresponding weights in \code{w}) are omitted from the computation. 
#' Default is \code{FALSE}. Additional missing values in \code{w} are not 
#' allowed.
#' @return A weighted variance.
weighted_quantile <- function(x, w, probs = probs, na.rm) {
  UseMethod("weighted_quantile", x)
}
#' @export
#' @method weighted_quantile ts
weighted_quantile.ts <- function(x, w, probs, na.rm = FALSE) {
  weighted_quantile(unclass(x), w, probs, na.rm)
}
#' @export
#' @method weighted_quantile mcmc
weighted_quantile.mcmc <- function(x, w, probs, na.rm = FALSE) {
  dimx <- dim(x)
  if (is.null(dimx)) {
    weighted_quantile.numeric(x, w, probs, na.rm)
  } else {
    if (length(dimx) == 2) {
      weighted_quantile.matrix(x, w, probs, na.rm)
    } else {
      weighted_quantile.numeric(x, w, probs, na.rm)
    }
  }
}

#' @export
#' @method weighted_quantile matrix
weighted_quantile.matrix <- function(x, w, probs, na.rm = FALSE) {
  if (nrow(x) != length(w)) 
    stop("Length of 'w' is not equal to the number of rows in 'x'. ")
  apply(x, 2, weighted_quantile.numeric, w = w, probs = probs, na.rm = na.rm)
}

#' @export
#' @method weighted_quantile numeric
weighted_quantile.numeric <- function(x, w, probs, na.rm = FALSE) {
  
  if (!(typeof(w) %in% c("integer", "double"))) {
    stop("Argument 'w' must be of type 'integer' or 'double'. ")
  }
  if (any(!is.finite(probs))) 
    stop("Nonfinite values in 'probs'. ")
  if (any(probs < 0 | probs > 1)) 
    stop("'probs' outside [0, 1].")
  if (length(na.rm) > 1 || !is.logical(na.rm)) 
    stop("Argument 'na.rm' should be a logical of length one.")
  
  if (any(nas <- is.na(x))) {
    if (na.rm) {
      x <- x[!nas]
      w <- w[!nas]
    } else {
      stop("Missing values or NaN in 'x' not allowed as 'na.rm' is FALSE.")
    }
  }
  if (anyNA(w))
    stop("Missing values or NaNs in 'w' not allowed.")
  
  if (any(dups <- duplicated(x))) {
    w <- cumsum(tapply(w, x, sum))
    w <- w / w[length(w)]
    x <- sort(x[!dups])
    
  } else {
    ord <- order(x)
    x <- x[ord]
    w <- cumsum(w[ord])
    w <- w / w[length(w)]
  }
  out <- numeric(length(probs))
  for (i in seq_along(probs)) {
    k <- which(w >= probs[i])[1]
    if (k == 1) {
      out[i] <- x[1]
    } else {
      out[i] <- x[k - 1] + 
        (x[k] - x[k - 1]) * (probs[i] - w[k - 1]) / (w[k] - w[k - 1])
    }
  }
  names(out) <- paste0(formatC(probs * 100, format = "fg"), "%")
  out
}


