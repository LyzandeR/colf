colf_nls <- function(formula, data, start = NULL, weights, trace = FALSE,
                     control, na.action, model = FALSE, lower, upper) {
 
 #convert formula to nls compatible formula
 model_ingredients <- construct_formula(formula, data)
 
 #construct starting values and add names
 if (is.null(start)) {
  start <- as.list(rep(1, length(model_ingredients$x_coef_names)))
  names(start) <- model_ingredients$x_coef_names
 }
 
 #run model
 nls(model_ingredients$model_formula, data = model_ingredients$model_data, start = start)
}
