#used in construct_formula() to form the rhs part of the formula
rhs_formula <- function(vars) {
 
 #length of names i.e. variables
 len <- length(vars)
 
 #construct coefs
 coefs <- paste0('param_', vars)
 
 #coef * variable pair
 coef_var <- paste(coefs, vars, sep = ' * ')
 
 #rhs of nls formula
 rhs_form <- paste(coef_var, collapse = ' + ')
 
 #add the names attribute
 attr(rhs_form, 'coefs') <- coefs
 
 rhs_form
 
}
