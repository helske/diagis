context("scalar tets")


test_that("scalar means work", {
  
  set.seed(123)
  x <- rnorm(10)
  w <- runif(10)
  expect_equal(weighted_mean(x, w), weighted.mean(x, w))
  expect_equal(running_weighted_mean(x, w)[1:2], c(x[1], weighted.mean(x[1:2], w[1:2])))
  expect_equal(running_mean(x)[1:2], c(x[1], mean(x[1:2])))
  
  expect_error(weighted_mean(1))
  expect_error(weighted_mean(1, 1:2))
  expect_error(weighted_mean(1:2, 1))
  expect_error(weighted_mean(1:2, "a"))
  expect_error(weighted_mean("a", 1))
  
  expect_error(running_mean(1:2, "a"))
  expect_error(running_mean("a", 1))
  
  expect_error(running_weighted_mean(1))
  expect_error(running_weighted_mean(1, 1:2))
  expect_error(running_weighted_mean(1:2, 1))
  expect_error(running_weighted_mean(1:2, "a"))
  expect_error(running_weighted_mean("a", 1))
  
})
