# #' Object Goodness-of-Fit Plot
# #' \code{OutliersPlot} A generic function used to produce plots showing outliers in the data from
# #' a model object.  The function invokes particular \code{\link{methods}}
# #' which depend on the \code{\link{class}} of the first argument.
# #'
# #' reports the goodness-of-fit of an object.
# #' @param object An object for which a summary is desired..
# #' @param ... Additional arguments affecting the goodness-of-fit displayed.
# #' @examples
# #'
# #' # linear regression
# #' x <- rnorm(10)
# #' y <- rnorm(10) + x
# #' mod <- lm(y ~ x)
# #' OutliersPlot(mod)
# #'
# #' # MDS - square
# #' library(smacof)
# #' data(breakfastDissimilarities)
# #' mdsInterval <- smacofSym(breakfastDissimilarities[[4]],
# #'     type = "interval", eps = 1e-12, itmax = 100000)
# #' OutliersPlot(mdsInterval)
# #' @export
# OutliersPlot <- function(object) {
#   UseMethod("OutliersPlot")
# }
#
# #' @describeIn OutliersPlot  Default outlier plot.
# #' @export
# OutliersPlot.default = function(object, ...) {
#   obs.fit = FittedAndObserved(object)
#   print(summary(object$observed))
#   linear.regression = lm(obs.fit$fitted ~ obs.fit$observed)
#   OutliersPlot.lm(linear.regression, ...)
# }
#
# #' @describeIn OutliersPlot  Outliers plot for a linear model.
# #' @export
# OutliersPlot.lm = function(object, ...) {
#   plot(object, which = 5)
# }
#
# #' @describeIn OutliersPlot  Outliers plot for a smacof object
# #' @export
# OutliersPlot.smacof = function(object, ...) {
#   plot(object, plot.type = "bubbleplot")
#   mtext("The larger the bubbles, the better the fit")
# }
#
# #' @describeIn OutliersPlot  Outliers plot for a rectangular smacof object
# #' @export
# OutliersPlot.smacofR = function(object, ...) {
#   plot(object, plot.type = "bubbleplot")
#   mtext("The larger the bubbles, the better the fit")
# }
