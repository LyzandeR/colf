#' Predict method for colf_nlxb
#'
#' Predict method for colf_nlxb
#'
#' \code{predict.colf_nlxb} will use the fit model to predict on a new data set.
#' 
#' When using predict.colf_nlxb make sure the column names and classes of the new data set are
#' the same as the data the model was trained on.
#'
#' @inheritParams predict.colf_nls
#' 
#' @param ... Currently not used
#'
#' @return A vector with the predictions
#' 
#' @examples 
#' mymod <- colf_nlxb(mpg ~ hp + cyl, mtcars)
#' 
#' #prediction
#' predict(mymod, mtcars)
#'
#' @export
predict.colf_nlxb <- predict.colf_nls