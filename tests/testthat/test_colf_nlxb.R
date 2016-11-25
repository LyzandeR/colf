context("test colf_nlxb is working")

test_that('function colf_nlxb works properly', {
 
 #works without failing if right arguments
 expect_error(mod_num <- colf_nlxb(mpg ~ cyl + disp, mtcars),
              NA)

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
 expect_error(mod_cat <- colf_nlxb(Sepal.Length ~ Sepal.Width + Species, iris),
              NA)
 
 #expect number of coefficients 4
 expect_equal(length(coef(mod_cat)), 4L)
 
 #see that methods do not return any errors
 expect_error(coef(mod_cat), NA)
 expect_error(summary(mod_cat), NA)
 expect_error(dump <- capture.output(print(mod_cat)), NA)
 expect_error(resid(mod_cat), NA)
 expect_error(fitted(mod_cat), NA)
 
 #variations of formula
 expect_error(mod_num <- colf_nlxb(mpg ~ ., mtcars),
              NA)
 
 expect_error(mod_num <- colf_nlxb(mpg ~ I(hp + cyl), mtcars),
              NA)
 
 expect_error(mod_num <- colf_nlxb(mpg ~ (hp + cyl + disp)^3, mtcars),
              NA)
 
 expect_error(mod_num <- colf_nlxb(mpg ~ hp:cyl, mtcars),
              NA)
 
 expect_error(mod_num <- colf_nlxb(mpg ~ hp * cyl, mtcars),
              NA)
 
 #lower and upper work
 expect_error(mod_num <- colf_nlxb(mpg ~ ., mtcars, upper = rep(2, 11), lower = rep(-0.5, 11)),
              NA)
 
 #start works with valid start values
 expect_error(mod_num <- colf_nlxb(mpg ~ ., mtcars, 
                                   upper = rep(2, 11), 
                                   lower = rep(-0.5, 11),
                                   start = rep(0.5, 11)),
              NA)
 
 #start fails if start values out of bounds
 expect_error(mod_num <- colf_nlxb(mpg ~ ., mtcars, 
                                   upper = rep(2, 11), 
                                   lower = rep(-0.5, 11),
                                   start = rep(5, 11)))
 
 
 #predict fails because hp could not be found
 expect_error({
  
  mod_num <- colf_nlxb(mpg ~ cyl + disp, mtcars)
  
  mtcars2 <- mtcars[c('cyl', 'hp')]
  
  predict(mod_num, mtcars2)
 })
 
 #predict fails because classes are different
 expect_error({
  
  mod_num <- colf_nlxb(mpg ~ cyl + disp, mtcars)
  
  mtcars2 <- mtcars[c('cyl', 'disp')]
  
  mtcars2$disp <- as.character(mtcars2$disp)
  
  predict(mod_num, mtcars2)
 })
 
 #make sure variation predictions work
 expect_error({
  
  mod_num <- colf_nlxb(mpg ~ I(cyl + disp), mtcars)
  
  mtcars2 <- mtcars[c('disp', 'cyl')]
  
  predict(mod_num, mtcars2)
 },
 NA)
 
 #make sure variation predictions work
 expect_error({
  
  mod_num <- colf_nlxb(mpg ~ (cyl + disp)^2, mtcars)
  
  mtcars2 <- mtcars[c('disp', 'cyl')]
  
  predict(mod_num, mtcars2)
 },
 NA)
 
})