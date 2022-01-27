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
  
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }
  if (missing(f) || missing(x)) {
    1 / sum((w / sum(w)) ^ 2)
  } else {
    
    if (length(x) != length(w)) stop("'x' and 'w' have unequal lengths. ")
    
    if (!is.function(f)) stop("Argument 'f' must be a function")
    fres <- f(x)
    if (length(fres) != length(w)) stop("'f(x)' should return vector with length equal to the length of 'w'. ")
    res <- abs(fres) * w
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
  
  if (!is.numeric(w) || any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  if (!any(w > 0)) {
    stop("No positive weights in 'w'.")
  }
  if (missing(f) || missing(x)) {
    
    csw <- cumsum(w)
    csw2 <- cumsum(w^2)
    1/(csw2/csw^2)
    
  } else {
    
    if (length(x) != length(w)) stop("'x' and 'w' have unequal lenghts. ")
    if (!is.function(f)) stop("Argument 'f' must be a function")
    fres <- f(x)
    if (length(fres) != length(w)) stop("'f(x)' should return vector with length equal to the length of 'w'. ")
    res <- abs(fres) * w
    csw <- cumsum(res)
    csw2 <- cumsum(res^2)
    1/(csw2/csw^2)
    
  }
}
