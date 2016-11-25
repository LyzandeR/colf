#' Non linear Least Squares on Linear Objective Function
#'
#' Non linear least squares using the port algorithm on a linear objective function.
#'
#' \code{colf_nls} uses nls, in an attempt to find the minimum of the residual sum of squares. 
#' The algorithm is applied on a linear objective function.
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
#' @inheritParams colf_nlxb
#'
#' @return Same as nls
#' 
#' @examples 
#' #no constraints
#' colf_nls(mpg ~ cyl + disp, mtcars)
#' 
#' #no intercept
#' colf_nls(mpg ~ 0 + cyl + disp, mtcars)
#' 
#' #including categorical variables. These will be dummified.
#' colf_nls(Sepal.Length ~ Sepal.Width + Species, iris)
#' 
#' #lower boundary will be replicated for all parameters 
#' colf_nls(Sepal.Length ~ Sepal.Width + Species, iris, lower = 0.5)
#' 
#' #species is categorical and contains 3 categories, thus we need to specify 4 lower bounds:
#' #the first one for the intercept.
#' #the second one for Sepal.Width 
#' #the two next for the dummy variables constructed from Species. 
#' colf_nls(Sepal.Length ~ Sepal.Width + Species, iris, lower = rep(0.5, 4))
#' 
#' @seealso \link[stats]{nls}
#'
#' @import stats nlmrt
#' @export
colf_nls <- function(formula, data, start = NULL, trace = FALSE,
                     control = NULL, na.action = c('na.omit', 'na.fail', 'na.exclude'), 
                     lower = -Inf, upper = Inf, ...) {
 
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
 nls_mod <- nls(model_ingredients$model_formula, 
                data = model_ingredients$model_data, 
                start = start,
                trace = trace,
                control = control,
                lower = lower,
                upper = upper,
                algorithm = 'port',
                ...)
 
 #include initial formula in the output
 nls_mod$formula <- formula
 
 #add the data as well
 nls_mod$model_data <- data
 
 #add class
 class(nls_mod) <- c('colf_nls', 'nls')
 
 #return
 nls_mod
 
}
