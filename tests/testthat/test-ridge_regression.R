library(testthat)
library(MASS)

context("Test the output of homework 2.")

test_that("You ridge regression function works in an easy case.", {

  data(iris)

  fit1 <- ridge_regression(Sepal.Length ~ ., iris, 0.1)

  fit2 <- lm.ridge(Sepal.Length ~ ., iris, lambda = 0.1)

  expect_equivalent(fit1[,1], coef(fit2), tolerance = 2e-1)
})
