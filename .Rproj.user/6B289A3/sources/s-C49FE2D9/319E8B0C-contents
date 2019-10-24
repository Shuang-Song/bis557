#' Conduct ridge regression.
#' @param form Regression formula.
#' @param data Dataset name.
#' @param lambda Shrinkage parameter, default 0, which means OLS regression.
#' @return The regression results.
#' @example ridge_regression(Sepal.Length~Sepal.Width, iris, 0.1)


ridge_regression <- function(form, data, lambda = 0) {
  #  rownames(data) <- NULL
  X <- model.matrix(form, data)
  # Y <- data[as.numeric(rownames(X)), as.character(form)[2]]
  Y <- data[[as.character(form)[2]]]
  ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(ret)$formula <- form
  class(ret) <- c(class(ret), "ridge_regression")
  ret
}

