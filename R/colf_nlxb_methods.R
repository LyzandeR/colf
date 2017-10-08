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


#' Residuals for colf_nlxb
#'
#' Residuals for colf_nlxb
#'
#' @param object A colf_nlxb object i.e. the result of running \code{colf_nlxb}
#' 
#' @param ... Currently not used
#'
#' @return A vector with the residuals
#' 
#' @examples 
#' mymod <- colf_nlxb(mpg ~ hp + cyl, mtcars)
#' 
#' #residuals
#' residuals(mymod)
#' resid(mymod)
#'
#' @export
residuals.colf_nlxb <- function(object, ...) {
 
 object$resid
 
}


#' Fitted values for colf_nlxb
#' 
#' Fitted values for colf_nlxb
#'
#' @param object A colf_nlxb object i.e. the result of running \code{colf_nlxb}
#' 
#' @param ... Currently not used
#'
#' @return  A vector with the fitted values
#' 
#' @examples 
#' mymod <- colf_nlxb(mpg ~ hp + cyl, mtcars)
#' 
#' #fitted values
#' fitted(mymod)
#'
#' @export
fitted.colf_nlxb <- function(object, ...) {
 
 predict.colf_nlxb(object, object$model_data)
 
}



#' Coefficients for colf_nlxb
#'
#' Coefficients for colf_nlxb
#'
#' @param object A colf_nlxb object i.e. the result of running \code{colf_nlxb}
#' 
#' @param ... Currently not used
#'
#' @return A vector with the coefficients
#' 
#' @examples 
#' mymod <- colf_nlxb(mpg ~ hp + cyl, mtcars)
#' 
#' #coefficients
#' coef(mymod)
#'
#' @export
coef.colf_nlxb <- function(object, ...) {
 
 coefficients <- object$coefficients
 coefficients
 
}

#' colf_nlxb Summary
#'
#' colf_nlxb Summary
#'
#' @param object A colf_nlxb object i.e. the result of running \code{colf_nlxb}
#' 
#' @param ... Currently not used
#'
#' @return The summary of the model
#' 
#' @examples 
#' mymod <- colf_nlxb(mpg ~ hp + cyl, mtcars)
#' 
#' #summary
#' summary(mymod)
#'
#' @export
summary.colf_nlxb <- function(object, ...) {
 
 {
  sumnlmrt <- list()
  smalltol <- .Machine$double.eps * 1000
  options(digits = 5)
  resname <- deparse(substitute(object))
  JJ <- object$jacobian
  res <- object$resid
  coeff <- object$coefficients
  pname <- names(coeff)
  npar <- length(coeff)
  lo <- object$lower
  if (is.null(lo)) 
   lo <- rep(-Inf, npar)
  up <- object$upper
  if (is.null(up)) 
   up <- rep(Inf, npar)
  mi <- object$maskidx
  mt <- rep(" ", npar)
  mt[mi] <- "M"
  bdmsk <- rep(1, npar)
  bdmsk[mi] <- 0
  ct <- rep(" ", npar)
  for (i in seq_along(coeff)) {
   if (lo[[i]] - coeff[[i]] > 0) {
    ct[[i]] <- "-"
    if (bdmsk[[i]] == 1) 
     bdmsk[[i]] <- -3
   }
   else {
    if (coeff[[i]] - lo[[i]] < smalltol * (abs(coeff[[i]]) + 
                                           smalltol)) {
     ct[[i]] <- "L"
     if (bdmsk[[i]] != 0) 
      bdmsk[[i]] <- -3
    }
   }
   if (coeff[[i]] - up[[i]] > 0) {
    ct[[i]] <- "+"
    if (bdmsk[[i]] == 1) 
     bdmsk[[i]] <- -1
   }
   else {
    if (up[[i]] - coeff[[i]] < smalltol * (abs(coeff[[i]]) + 
                                           smalltol)) {
     ct[[i]] <- "U"
     if (bdmsk[[i]] != 0) 
      bdmsk[[i]] <- -1
    }
   }
  }
  ss <- object$ssquares
  nobs <- length(res)
  ndof <- nobs - npar
  if (ndof <= 0) {
   if (ndof < 0) {
    stop(paste("Inadmissible degrees of freedom =", ndof, 
               sep = ""))
   }
   else {
    sighat2 <- Inf
   }
  }
  else {
   sighat2 <- as.numeric(ss)/(ndof)
  }
  dec <- svd(JJ)
  U <- dec$u
  V <- dec$v
  Sd <- dec$d
  if (min(Sd) <= smalltol * max(Sd)) {
   SEs <- rep(NA, npar)
  }
  else {
   Sinv <- 1/Sd
   Sinv[which(bdmsk != 1)] <- 0
   if (npar > 1) {
    VS <- crossprod(t(V), diag(Sinv))
   }
   else {
    VS <- V * Sinv
   }
   Jinv <- crossprod(t(VS))
   var <- Jinv * sighat2
   SEs <- sqrt(diag(var))
  }
  gr <- crossprod(JJ, res)
  if (any(is.na(SEs))) {
   tstat <- rep(NA, npar)
  }
  else {
   if (any(SEs == 0)) {
    tstat <- rep(0, npar)
   }
   else {
    tstat <- coeff/SEs
   }
  }
  pval <- 2 * (1 - pt(abs(tstat), df = ndof))
  data_frame_to_print <- data.frame(pname,
                                    coeff,
                                    SEs,
                                    tstat,
                                    pval,
                                    gr,
                                    Sd)
  row.names(data_frame_to_print) <- NULL
  names(data_frame_to_print) <- c('name', 'coeff', 'SEs', 'tstat', 'pval', 'gradient', 'JSingval')
  object <- list(resname = resname, ssquares = ss, nobs = nobs, 
                 coeff = coeff, ct = ct, mt = mt, SEs = SEs, tstat = tstat, 
                 pval = pval, Sd = Sd, gr = gr, jeval = object$jeval, 
                 feval = object$feval,
                 data_frame_to_print = data_frame_to_print)
  object
 }
 
}


#' colf_nlxb Print method
#'
#' colf_nlxb Print method
#'
#' @param x A colf_nlxb object i.e. the result of running \code{colf_nlxb}
#' 
#' @param ... Currently not used
#'
#' @return Printing the colf_nlxb object
#' 
#' @examples 
#' mymod <- colf_nlxb(mpg ~ hp + cyl, mtcars)
#' 
#' #print
#' print(mymod)
#'
#' @export
print.colf_nlxb <- function(x, ...) {
 
 xx <- summary(x)
 with(xx, {
  cat("nlsr class object:", resname, "\n")
  pname <- names(coeff)
  npar <- length(coeff)
  cat("residual sumsquares = ", ssquares, " on ", nobs, 
      "observations\n")
  cat("    after ", jeval, "   Jacobian and ", feval, "function evaluations\n")
 })
 print(xx$data_frame_to_print)
 invisible(x)

}
