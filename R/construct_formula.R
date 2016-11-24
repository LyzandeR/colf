#' Construct an nls-compatible formula from an lm style formula
#'
#' Construct an nls-compatible formula from an lm style formula
#'
#' \code{construct_formula} creates the parameters needed for the formula to be compatible with nls
#' style functions.
#' 
#' \code{construct_formula} will make syntactically valid names (if applicable) otherwise the 
#' optimizers will fail. To make these names \code{make.names} is used. Check examples.
#'
#' @inheritParams colf_nlxb
#'
#' @return A list of three elements:
#' \itemize{
#'   \item model_formula - An nls compatible formula
#'   \item model_data - The modelling set created (inlcuding dummy variables, if any)
#'   \item x_param_names - The names of the parameters
#' 
#' }
#' 
#' @examples 
#' #simple syntax
#' construct_formula(mpg ~ hp + cyl, mtcars)
#' 
#' #example of make.names to create syntactically valid names
#' make.names('(foo/^@bar)')
#' 
#' #function will create syntactically valid names (if applicable) 
#' #otherwise the optimizers will fail
#' construct_formula(mpg ~ I(hp + cyl), mtcars)
#' construct_formula(mpg ~ (hp + cyl + disp)^3, mtcars)
#' 
#' @seealso \link[stats]{nls}, \link{make.names}
#'
#' @export
construct_formula <- function(formula, data) {
 
 #model frame of data
 mf <- model.frame(formula, data)
 
 #extract the response
 y <- model.response(mf, 'numeric')
 
 #get the terms of the model frame
 mt <- attr(mf, "terms")
 
 #construct the model matrix i.e. rhs of formula
 mm <- model.matrix(mt, mf)
 
 #fix names because it nls cannot work with syntactically valid names
 colnames(mm) <- make.names(colnames(mm), unique = TRUE)
 
 #get the names
 x_names <- colnames(mm)
 y_name <- names(mf)[1]
 
 #construct the right hand side of nls formula
 model_rhs_formula <- rhs_formula(x_names)
 
 #create nls_formula
 model_formula <- as.formula(paste0(y_name, ' ~ ', model_rhs_formula))
 
 #construct complete data for nls as a list - fastest way
 model_data <- cbind(y, mm)
 model_data <- lapply(seq_len(ncol(model_data)), function(i) model_data[, i])
 names(model_data) <- c(y_name, x_names)
 
 list(model_formula = model_formula, 
      model_data = model_data, 
      x_coef_names = attr(model_rhs_formula, 'coefs'))
 
}