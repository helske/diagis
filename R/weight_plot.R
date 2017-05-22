#' Diagnostic plot of importance sampling weights
#'
#' Function \code{weight_plot} plots four figures given the weight vector \code{w}: 
#' Plot of largest weights, sorted graph of all weights, running variance estimate of weights, 
#' and running effective sample size estimate of weights.
#' 
#' @importFrom ggplot2 ggplot geom_point geom_line ggtitle scale_y_continuous aes aes_
#' @importFrom gridExtra grid.arrange
#' @param w Vector of weights
#' @export
#' @examples
#' #' importance sampling from too narrow distribution
#' #' weights have infinite variance
#' set.seed(1)
#' x_inf <- rnorm(1000, sd = 0.1)
#' w_inf <- dnorm(x_inf) / dnorm(x_inf, 0, 0.1)
#' weight_plot(w_inf)
#' x_opt <- rnorm(1000, sd = sqrt(2))
#' w_opt <- dnorm(x_opt) / dnorm(x_opt, 0, sqrt(2))
#' weight_plot(w_opt)
weight_plot <- function(w){
  
  if (any(w < 0)) stop ("Weight vector 'w' must contain only non-negative values. ")
  
  ind <- w > sort(w, decreasing = TRUE)[min(100, length(w))]
  dat <- data.frame(weight = w[ind], index = which(ind))
  p1 <- ggplot(data = dat, aes_(x = ~index, y = ~weight)) + geom_point() + 
    ggtitle(paste0(min(100, length(w)), " largest weights"))
  index <- seq_along(w)
  p2 <- ggplot(mapping = aes(x = index, y = sort(w))) + geom_point() + 
    scale_y_continuous("variance") + 
    ggtitle("sorted weights")
  p3 <- ggplot(mapping = aes(x = index, y = running_var(w))) + geom_line() + 
    scale_y_continuous("value") + ggtitle("running variance of weights")
  p4 <- ggplot(mapping = aes(x = index, y = running_ess(w))) + geom_line() + 
    scale_y_continuous("value") + ggtitle("running ESS")
  grid.arrange(p1, p2, p3, p4, ncol=2)
}
