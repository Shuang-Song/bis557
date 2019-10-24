library(testthat)
library(MASS)
library(glmnet)
library(rsample)
library(doParallel)
library(casl)
library(dplyr)
library(crayon)

context("Test the output of homework 2.")

test_that("You oitimze lambda function works in an easy case.", {

  data(iris)

  l1 <- optimize_lambda(iris, Sepal.Length ~ ., seq(0, 0.5, 0.005))

  l2 <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris), as.matrix(iris[,1]), alpha = 0, lambda=seq(0, 0.5, 0.005))

  expect_equivalent(l1, l2$lambda.min, tolerance = 1e-2)
})

