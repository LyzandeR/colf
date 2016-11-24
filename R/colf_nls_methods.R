#' Predict method for colf_nls
#'
#' Predict method for colf_nls
#'
#' \code{predict.colf_nls} will use the fit model to predict on a new data set.
#' 
#' When using predict.colf_nls make sure the column names and classes of the new data set are
#' the same as the data the model was trained on.
#'  
#' @param object A colf_nls object
#' 
#' @param newdata A new data.frame which contains the same column names and classes as the original
#' data.frame
#' 
#' @param ... Currently not used
#'
#' @return A vector with the predictions
#' 
#' @examples 
#' mymod <- colf_nls(mpg ~ hp + cyl, mtcars)
#' 
#' #prediction
#' predict(mymod, mtcars)
#'
#' @export
predict.colf_nls <- function(object, newdata, ...) {
 
 #get formula from colf model
 formula <- object$formula
 
 #set a fake response so that construct_formula does not fail
 y <- all.vars(formula)[1]
 newdata[[y]] <- 0
 
 #xnames
 x_names <- labels(terms(formula))
 
 if (all(!x_names %in% names(newdata))) {
  stop(paste(paste(x_names, collapse = ', ')), ' do not exist in newdata')
 }
 
 if (identical(class(newdata[x_names]), object$colclasses)) {
  stop(paste(paste(x_names, collapse = ', ')), ' do not have the same
       classes between newdata and original data')
 }
 
 #construct nls compatible formula
 b <- construct_formula(formula, newdata)
 
 #remove response
 b$model_data[[y]] <- NULL
 
 #the sum of each row is a pred
 row_calc <- Map('*', b$model_data, unname(coef(object)))
 
 #convert to data.frame
 rowSums(data.frame(row_calc))
 }