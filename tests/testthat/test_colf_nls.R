context("test colf_nls is working")

test_that('function colf_nls works properly', {
 
 #works without failing if right arguments
 expect_error(mod_num <- colf_nls(mpg ~ cyl + disp, mtcars),
              NA)
 
 #make sure data classes and formula are provided
 expect_true(!is.null(mod_num$formula) & !is.null(mod_num$colclasses))
 
 #expect number of coefficients 3
 expect_equal(length(coef(mod_num)), 3L)
 
 
 #estimates the same coefficients as lm for simple case
 expect_true(all.equal(as.numeric(coef(mod_num)),
                       as.numeric(coef(lm(mpg ~ cyl + disp, mtcars))),
                       tolerance = 0.01))
 
 #predict works with no errors
 expect_error(predict(mod_num, mtcars), 
              NA)
 
 #if wrong data set then error
 expect_error(predict(mod_num, iris))
 
 #works with categorical
 expect_error(mod_cat <- colf_nls(Sepal.Length ~ Sepal.Width + Species, iris),
              NA)
 
 #expect number of coefficients 4
 expect_equal(length(coef(mod_cat)), 4L)
 
 #see that methods do not return any errors
 expect_error(coef(mod_cat), NA)
 expect_error(summary(mod_cat), NA)
 expect_error(dump <- capture.output(print(mod_cat)), NA)
 expect_error(resid(mod_cat), NA)
 expect_error(fitted(mod_cat), NA)
 
 #make sure different variations of formula work
 
 #make sure lower and upper work
 expect_error(mod_num <- colf_nls(mpg ~ ., mtcars),
              NA)
 
 #make sure start works
 
})