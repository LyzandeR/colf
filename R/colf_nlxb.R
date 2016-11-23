#' Nash Variant of the Marquardt algorithm on a linear objective function
#'
#' Non linear least squares solution via qr linear solver on a linear objective function.
#'
#' \code{colf_nlxb} uses Nash's (Nash, 1979) variant of the Marquardt algorithm, in an attempt to
#' find the minimum of the residual sum of squares.
#'
#' @param formula The formula. This has the same syntax as formula in \code{lm}. Look at details.
#' 
#' @param data A data frame containing the data of the variables in the formula.
#' 
#' @param start A named parameter list. Check details.
#' 
#' @param trace Logical TRUE if we want intermediate progress to be reported. Default is FALSE.
#' 
#' @param lower Lower bounds on the parameters. If a single number, this will be applied to all 
#'              parameters. Default -Inf.
#'              
#' @param upper Upper bounds on the parameters. If a single number, this will be applied to all 
#' parameters. Default Inf.  
#' 
#' @param masked Character vector of quoted parameter names. These parameters will NOT be altered 
#' by the algorithm.     
#' 
#' @param control A list of controls for the algorithm. These are:
#'
#' @return Same as nlxb
#'
#' @export
colf_nlxb <- function(formula, data, start = NULL, trace = FALSE, lower = -Inf, upper = Inf,
                      masked = NULL, control) {
 
 #convert formula to nls compatible formula
 model_ingredients <- construct_formula(formula, data)
 
 #construct starting values and add names
 if (is.null(start)) {
  start <- as.list(rep(1, length(model_ingredients$x_coef_names)))
  names(start) <- model_ingredients$x_coef_names
 }
 
 #run model
 nlxb(model_ingredients$model_formula, data = model_ingredients$model_data, start = start)
 
}