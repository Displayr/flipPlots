#brand.associations = system.file("extdata", "brandassoc.csv", package="flipPlots")



brand.associations <- read.csv("C:/Users/Tim/Documents/GitHub/flipPlots/inst/extdata/brandassoc.csv",
                              row.names = 1)
brand.associations <- matrix(as.numeric(as.matrix(z)), nrow = nrow(z),
                             dimnames = dimnames(z))

brand.sim <- 1 - cor(t(brand.associations))
library(MASS)



LabeledScatterPlot(isoMDS(brand.sim)$points)


#' # MDS - square
#' library(smacof)
#' data(breakfastDissimilarities)
#' mdsInterval <- smacofSym(breakfastDissimilarities[[4]],
#'     type = "interval", eps = 1e-12, itmax = 100000)
#' LabeledScatterPlot(mdsInterval, title = "Interval-scale MDS of Breakfast Dissimilarities")
#'
#' data(colaPerceptions)
#' LabeledScatterPlot(colaPerceptions[,c(7,8)], title = "Scatterplot of perceptions data",
#'                           auto.tidy = TRUE, auto.color = 5, fixed.aspect = TRUE)
