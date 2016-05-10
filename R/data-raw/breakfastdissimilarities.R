#data = system.file("extdata", "Cola_perceptions.csv", package="flipGenerics")
data = read.csv("http://surveyanalysis.org/images/2/27/Breakfast_Dissimilarities.csv")
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
breakfast.dissimilarities <- breakfastDissimilarities
devtools::use_data(breakfast.dissimilarities, internal = FALSE, overwrite = TRUE)

# MDS - square
library(smacof)
data(breakfast.dissimilarities)
mdsInterval <- smacofSym(breakfast.dissimilarities[[4]],
     type = "interval", eps = 1e-12, itmax = 100000)

LabeledScatterPlot(mdsInterval)

)
