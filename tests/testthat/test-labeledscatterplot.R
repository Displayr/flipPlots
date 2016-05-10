context("Labeledscatterplot")

data("brand.associations")
brand.correlations <- cor(t(brand.associations))
brand.sim <- 1 - brand.correlations
zcoords <- as.data.frame(MASS::isoMDS(brand.sim)$points)
colnames(zcoords) <- c("A","N")

## Labeled Scatterplot regression tests
# default
LabeledScatterPlot(zcoords)

InteractiveLabeledScatterPlot(zcoords)

# MDS - square
library(smacof)
data(breakfast.dissimilarities)
mdsInterval <- smacofSym(breakfast.dissimilarities[[4]],
     type = "interval", eps = 1e-12, itmax = 100000)

LabeledScatterPlot(mdsInterval)



data(csd.perceptions)
z <- csd.perceptions[c(5:8, 1:4),3:4]

#dimnames(z)[[2]] <- c("col1","col2")#c("Dimension 1","Dimension 2")#paste0("`", dimnames(z)[[2]], "`")
#dimnames(z)[[1]] <- LETTERS[1:nrow(z)]

#csd.perceptions[,3:4]
LabeledScatterPlot(z, point.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, fixed.aspect = TRUE, group = rep(1:2,4))

InteractiveLabeledScatterPlot(z, fixed.aspect = FALSE)

LabeledScatterPlot(z, point.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12,fixed.aspect = TRUE)
LabeledScatterPlot(z, point.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12,  fixed.aspect = FALSE)
LabeledScatterPlot(z, point.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12,fixed.aspect = FALSE)

LabeledScatterPlot(z, fixed.aspect = FALSE, label.font.size = 20, point.size = 3, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12)
LabeledScatterPlot(z, fixed.aspect = FALSE, label.font.size = 20, point.size = 3, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12)

LabeledScatterPlot(z, point.size = 3, label.font.size = 2, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = TRUE)



LabeledScatterPlot(z, point.size = 3, label.font.size = 50, legend.font.size = 10, axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE)


z = matrix(c(1:10,1:10), 10, dimnames = list(c(rep("jJ", 3), rep("Big Dog", 3), rep("Stupendeously, Stupendeously big-canine", 4)), c("a1", "a2")))
LabeledScatterPlot(z, point.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE, fixed.aspect = FALSE)
LabeledScatterPlot(z, point.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = TRUE, fixed.aspect = FALSE)
LabeledScatterPlot(z, point.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE, fixed.aspect = FALSE)


