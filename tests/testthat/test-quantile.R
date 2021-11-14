context("quantile tests")

test_that("weighted quantile works", {
  
  set.seed(123)
  x <- rnorm(10)
  w <- runif(10)
  p <- seq(0, 1, by = 0.1)
  expect_equal(weighted_quantile(x, rep(1, length(x)), p), 
    quantile(x, p, type = 4))
  expect_error(weighted_qauntile(x, w+NA, p))
  expect_error(weighted_qauntile(x, w))
  expect_error(weighted_qauntile(x, w, -1))
  expect_error(weighted_qauntile(1))
  expect_error(weighted_qauntile(1, 1:2))
  expect_error(weighted_qauntile(1, "a"))
  expect_error(weighted_qauntile("a", 1))
  
  set.seed(1)
  x <- rnorm(1e5)
  w <- runif(1e5)
  expect_equal(weighted_quantile(x, w, p), quantile(x, p, type = 4), tol = 0.05)
})
