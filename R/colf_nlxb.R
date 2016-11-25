#' Nash Variant of the Marquardt algorithm on a linear objective function
#'
#' Non linear least squares solution via qr linear solver on a linear objective function.
#'
#' \code{colf_nlxb} uses Nash's (Nash, 1979) variant of the Marquardt algorithm, in an attempt to
#' find the minimum of the residual sum of squares. The algorithm is applied on a linear objective
#' function.
#' 
#' The function provides an easy way to apply the optimizer on a linear objective function in a 
#' similar way to \code{lm}. 
#' 
#' start, lower and upper, if provided, can be either an atomic vector which has the same length as
#' the number of parameters or a single number which will be replicated to match the length of the 
#' parameters. If categorical variables exist in the function these will be dummified. Out of one
#' categorical variable, n - 1 will be created where n is the total number of categories in the 
#' variable. This needs to be taken into account when providing an atomic vector for start, lower or
#' upper. Also, as with \code{lm} an intercept will be added which also needs to be taken into 
#' account. 
#'
#' @param formula The formula. This has the same syntax and supports the same features as the 
#' formula in \code{lm}. See examples.
#' 
#' @param data A data frame containing the data of the variables in the formula.
#' 
#' @param start An atomic vector of same length as the number of parameters. If not provided a 
#' cheap guess will be made. If categorical variables are included these need to be takent into 
#' consideration as number of categories minus one. See examples and details.
#' 
#' @param trace Logical. Defaults to FALSE. Set to TRUE if you want the intermediate progress to be 
#' reported
#' 
#' @param lower Lower bounds of the parameters (atomic vector). If a single number, this will be 
#' applied to all parameters. Defaults to -Inf (unconstrained).
#'              
#' @param upper Upper bounds of the parameters (atomic vector). If a single number, this will be 
#' applied to all parameters. Defaults to Inf (unconstrained).  
#' 
#' @param masked Character vector of parameter names. These parameters will not be altered 
#' by the algorithm.     
#' 
#' @param na.action A function which indicates what should happen if NAs are present in the data
#' set. Defaults to options('na.action'). na.fail, or na.exclude can be used.
#' 
#' @param ... Other arguments passed on to optimiser
#' 
#' @param control A list of controls for the algorithm. These are:
#' \itemize{
#'   \item watch - Monitor progress. Logical, defaults to FALSE
#'   \item phi - Adds phi*identity to Jacobian inner product. Defaults to 1.
#'   \item lamda - Initial Marquardt adjustment. Defaults to 0.0001.
#'   \item offset - Shift to test floating point equality. Defaults to 100.
#'   \item laminc - Factor to use to increase lamda. Defaults to 10.
#'   \item lamdec - Factor to use to decrease lamda (lamdec / laminc). Defaults to 4.
#'   \item femax - Maximum evaluations of sum of squares function. Defaults to 10000.
#'   \item jemax - Maximum evaluations of the Jacobian. Defaults to 5000.
#'   \item rofftest - Use a termination of the relative offset orthogonality type. 
#'   \item smallsstest - Exit the function if the sum of squares falls below 
#'   (100 * .Machine$double.eps)^4 times the initial sumsquares. Defaults to TRUE.
#' }
#'
#' @return Same as nlxb
#' 
#' @examples 
#' #no constraints
#' colf_nlxb(mpg ~ cyl + disp, mtcars)
#' 
#' #no intercept
#' colf_nlxb(mpg ~ 0 + cyl + disp, mtcars)
#' 
#' #including categorical variables. These will be dummified.
#' colf_nlxb(Sepal.Length ~ Sepal.Width + Species, iris)
#' 
#' #lower boundary will be replicated for all parameters 
#' colf_nlxb(Sepal.Length ~ Sepal.Width + Species, iris, lower = 0.5)
#' 
#' #species is categorical and contains 3 categories, thus we need to specify 4 lower bounds:
#' #the first one for the intercept.
#' #the second one for Sepal.Width 
#' #the two next for the dummy variables constructed from Species. 
#' colf_nlxb(Sepal.Length ~ Sepal.Width + Species, iris, lower = rep(0.5, 4))
#' 
#' @seealso \link[nlmrt]{nlxb}
#'
#' @export
colf_nlxb <- function(formula, data, start = NULL, trace = FALSE, lower = -Inf, upper = Inf,
                      na.action = c('na.omit', 'na.fail', 'na.exclude'), masked = NULL, 
                      control = NULL, ...) {
 
 #na.action
 na.action <- match.arg(na.action)
 data <- get(na.action)(data)
 
 #convert formula to nls compatible formula
 model_ingredients <- construct_formula(formula, data)
 
 #construct starting values and add names
 if (is.null(start)) {
  start <- as.list(rep(1, length(model_ingredients$x_coef_names)))
  names(start) <- model_ingredients$x_coef_names
 } else {
  start <- as.list(start)
  names(start) <- model_ingredients$x_coef_names
 }
 
 #run model
 nlxb_mod <- nlxb(model_ingredients$model_formula, 
                  data = model_ingredients$model_data, 
                  start = start,
                  trace = trace,
                  lower = lower,
                  upper = upper,
                  masked = masked,
                  control = control,
                  ...)
 
 #include initial formula in the output
 nlxb_mod$formula <- formula
 
 #add the data as well
 nlxb_mod$model_data <- data
 
 #add class
 class(nlxb_mod) <- c('colf_nlxb', 'nlmrt')
 
 #return
 nlxb_mod
 
}