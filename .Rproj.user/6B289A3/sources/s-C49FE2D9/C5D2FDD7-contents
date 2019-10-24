#' Calculate optimal lambda.
#' @import rsample
#' @import doParallel
#' @import casl
#' @import dplyr
#' @import crayon
#' @param data Dataframe.
#' @param form Formula.
#' @param lambdas A vector of lambda.
#' @fold Cross-validation folds.
#' @example optimize_lambda(iris, Sepal.Length~Sepal.Width, seq(0, 0.5, 0.005))

optimize_lambda <- function(data, form, lambdas, fold=10) {
  registerDoParallel(6)
  folds <- vfold_cv(data, fold)
  rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      casl_util_rmse(testing(folds$splits[[i]])[[as.character(form[2])]],
                     predict(ridge_regression(form, training(folds$splits[[i]]), lambda), testing(folds$splits[[i]])))
    }
  }

  edf <- tibble(mean = apply(rmses, 1, mean),
                sd = apply(rmses, 1, sd),
                lambda = lambdas,) %>%
    mutate(upper = mean + 2 * sd / sqrt(nrow(.)),
           lower = mean - 2 * sd / sqrt(nrow(.)))
  return(edf$lambda[edf$mean==min(edf$mean)])
}
