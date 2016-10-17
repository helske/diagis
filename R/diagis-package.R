#' Auxiliary functions and diagnostic plots for importance sampling
#'
#' This package contains functions computing weighted summaries and diagonostic plots 
#' for importance sampling problems. 
#' 
#' @docType package
#' @name diagis
#' @aliases diagis
#' @importFrom Rcpp evalCpp
#' @useDynLib diagis
#' @examples
#' # simple importance sampling example
#' # true distribution is a standard normal:
#' p <- function(x) dnorm(x)
#' # proposal distribution is normal with sd s
#' q <- function(x, s) dnorm(x, 0, s)
#' 
#' # IS weights have finite variance only if s^2 > 1/2
#' # variance is s/(2-1/s^2)^(3/2)
#' 
#' #optimimal case
#' set.seed(42)
#' s <- sqrt(2)
#' x <- rnorm(10000, sd = s)
#' w <- p(x) / q(x, s) 
#' weighted_mean(x, w)
#' weighted_var(x, w)
#' s <- 0.25
#' x2 <- rnorm(10000, sd = s)
#' w2 <- p(x2) / q(x2, s)
#' weighted_mean(x2, w2)
#' weighted_var(x2, w2)
#' weighted_mean(x2, w2) #hmm...
#' weighted_var(x2, w2) #!!
#' # check recursive variance estimate of weights
#' ts.plot(running_var(w2))
NULL
