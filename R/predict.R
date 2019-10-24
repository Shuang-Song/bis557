#' Predict based on ridge regression parameters.
#' @example ridge_predict(x, iris)


predict.ridge_regression <- function(object, ...) {
  dots <- list(...)
  x_frame <- dots[[1]]
  if (!is.data.frame(x_frame)) {
    stop(red("The first argument should be a data.frame of values",
             "to predict"))
  }
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}
