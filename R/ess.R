#' Effective sample size
#'
#' Computes the effective sample size (ESS) of importance sampling estimator.
#' 
#' @export
#' @param w A numeric vector of non-negative weights.
#' @param f A function used in computing \code{f}-specific ESS.
#' @param x A numeric vector of samples used to generate \code{w}. Used for computing \code{f(x)}.
#' @return An effective sample size estimate.
ess <- function(w, f, x){
  if (missing(f) || missing(x)) {
    1 / sum((w / sum(w)) ^ 2)
  } else {
    if (!is.function(f)) stop("Argument 'f' must be a function")
    res <- abs(f(x)) * w
    res <- res / sum(res)
    1 / sum(res ^ 2)
  }
}
#' Running effective sample size
#'
#' Computes and returns the running estimate of effective sample size (ESS) of 
#' importance sampling estimator.
#' 
#' @export
#' @param w A numeric vector of non-negative weights.
#' @param f A function used in computing \code{f}-specific ESS.
#' @param x A numeric vector of samples used to generate \code{w}. Used for computing \code{f(x)}.
#' @return An effective sample size estimate.
running_ess <- function(w, f, x){
  if (missing(f) || missing(x)) {
    csw <- cumsum(w)
    sapply(seq_along(w), function(i) 1/sum((w[1:i]/csw[i])^2))
  } else {
    if (!is.function(f)) stop("Argument 'f' must be a function")
    res <- abs(f(x)) * w
    csw <- cumsum(res)
    sapply(seq_along(res), function(i) 1/sum((res[1:i]/csw[i])^2))
  }
}
