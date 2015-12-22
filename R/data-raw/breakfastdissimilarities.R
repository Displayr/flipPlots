data = system.file("extdata", "Cola_perceptions.csv", package="flipGenerics")
#data = read.csv("http://surveyanalysis.org/images/2/27/Breakfast_Dissimilarities.csv")
breakfasts = names(data[,-1])
breakfastDissimilarities = vector("list", 4)
n = 4
k = 15
firstRow = seq(1,nrow(data),k)
names(breakfastDissimilarities) = data[firstRow,1]
for (i in 1:4) {
  zmat = as.matrix(data[firstRow[i]:(firstRow[i] + k - 1),-1])
  dimnames(zmat) = list(breakfasts,breakfasts)
  breakfastDissimilarities[[i]] = as.dist(zmat)
}
breakfastDissimilarities


devtools::use_data(breakfastDissimilarities, internal = FALSE, overwrite = TRUE)




#'  @describeIn LabeledScatterPlot  Labeled scatterplot of smacof object
#'  @export
LabeledScatterPlot.smacof = function(object, ...)
{
    LabeledScatterPlot.default(object$conf, fixed.aspect = TRUE, ...)
}

#'  @describeIn LabeledScatterPlot  Labeled scatterplot of smacofB object
#'  @export
LabeledScatterPlot.smacofB = function(object, ...)
{
    LabeledScatterPlot.default(object$conf, fixed.aspect = TRUE, ...)
}


#'  @describeIn LabeledScatterPlot  Labeled scatterplot of smacofR object
#'  @export
LabeledScatterPlot.smacofR = function(object, ...) # # row.description = "Rows", column.description = "Columns",  ...) {
{
    coords = rbind(object$conf.row, object$conf.col)
    group = c(rep(row.description, length(object$spp.row)),rep(column.description, length(object$spp.col)))
    LabeledScatterPlot.default(coords, fixed.aspect = TRUE, group = group, ...)
}


#'
# # MDS - square
# data(breakfastDissimilarities)
#     type = "interval", eps = 1e-12, itmax = 100000)
# LabeledScatterPlot(mdsInterval, title = "Interval-scale MDS of Breakfast Dissimilarities")
#
# data(colaPerceptions)
# LabeledScatterPlot(colaPerceptions[,c(7,8)], title = "Scatterplot of perceptions data",
#                           auto.tidy = TRUE, auto.color = 5, fixed.aspect = TRUE)
#' @export
LabeledScatterPlot <- function(coords, ...) UseMethod("LabeledScatterPlot")

#' @describeIn LabeledScatterPlot  Default labeled scatterplot
#'
#'
