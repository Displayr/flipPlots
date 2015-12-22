#brand.associations = system.file("extdata", "brandassoc.csv", package="flipPlots")
brand.associations <- read.csv("C:/Users/Tim/Documents/GitHub/flipPlots/inst/extdata/brandassoc.csv",
                              row.names = 1)
devtools::use_data(brand.associations, internal = FALSE, overwrite = TRUE)
#brand.associations <- matrix(as.numeric(as.matrix(z)), nrow = nrow(z),
#                             dimnames = dimnames(z))


brand.correlations <- cor(t(brand.associations))
brand.sim <- 1 - brand.correlations
zcoords <- MASS::isoMDS(brand.sim)$points

## Interactive scatter plot regression tests
# default
InteractiveLabeledScatterPlot(zcoords)

# row labels
InteractiveLabeledScatterPlot(zcoords, row.labels = c(LETTERS[1:26], 1:16))

# row and column labels
InteractiveLabeledScatterPlot(zcoords, row.labels = c(LETTERS[1:26], 1:16), column.labels = LETTERS[1:2])

# changing the colors
InteractiveLabeledScatterPlot(zcoords, colors = q.colors[1:3])

# turning the colors off
InteractiveLabeledScatterPlot(zcoords, auto.color = FALSE)

# fixing the aspect ratio
InteractiveLabeledScatterPlot(zcoords,fixed.aspect = TRUE)

# group
InteractiveLabeledScatterPlot(zcoords, group = rep(1:2, 21))

# group name
InteractiveLabeledScatterPlot(zcoords, group = rep(1:2, 21), group.name = "Groups")

# Tooltips
tooltips <- CreateInteractiveScatterplotTooltips(brand.correlations)
InteractiveLabeledScatterPlot(zcoords, group = rep(1:2, 21), group.name = "Groups", tooltip.text = tooltips)

## Labeled Scatterplot regression tests
# default
LabeledScatterPlot(zcoords)

# no auto-tidy
LabeledScatterPlot(zcoords, auto.tidy = FALSE)

# row labels
LabeledScatterPlot(zcoords, row.labels = c(LETTERS[1:26], 1:16))

# row and column labels
LabeledScatterPlot(zcoords, row.labels = c(LETTERS[1:26], 1:16), column.labels = LETTERS[1:2])

# changing the colors
LabeledScatterPlot(zcoords, colors = q.colors[1:3])

# turning the colors off
LabeledScatterPlot(zcoords, auto.color = FALSE)

# fixing the aspect ratio
LabeledScatterPlot(zcoords,fixed.aspect = TRUE)

# group
LabeledScatterPlot(zcoords, group = rep(1:2, 21))

# group name
LabeledScatterPlot(zcoords, group = rep(1:2, 21), group.name = "Groups")

# Legend size
LabeledScatterPlot(zcoords, group = rep(1:2, 21), group.name = "Groups", legend.size = 20)

# use spaces as space substitution
LabeledScatterPlot(zcoords, general.color = "red")

# Point size
LabeledScatterPlot(zcoords, point.size = 10)

# Label size
LabeledScatterPlot(zcoords, label.size = 20)

# cruder tstep
LabeledScatterPlot(zcoords, label.size = 20, tstep = 1)

# cruder rstep
LabeledScatterPlot(zcoords, label.size = 20, rstep = 1)

# finer tstep and rstep
LabeledScatterPlot(zcoords, label.size = 20, tstep = 0.01, rstep = 0.01)

# finer tstep and rstep
LabeledScatterPlot(zcoords, label.size = 20, tstep = 0.01, rstep = 0.01)

# Permitting overlap
LabeledScatterPlot(zcoords, label.size = 20, overlap.fudge = 2)

# Requiring spaces between things
LabeledScatterPlot(zcoords, label.size = 20, overlap.fudge = 0.5)

# Axis title size
LabeledScatterPlot(zcoords, axis.title.size = 20)

# Axis title
LabeledScatterPlot(zcoords, main = "The big title")

# Axis title size
LabeledScatterPlot(zcoords, main = "The big title", main.size = 24)
