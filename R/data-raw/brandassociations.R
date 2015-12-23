#brand.associations = system.file("extdata", "brandassoc.csv", package="flipPlots")
brand.associations <- read.csv("C:/Users/Tim/Documents/GitHub/flipPlots/inst/extdata/brandassoc.csv",
                              row.names = 1)
devtools::use_data(brand.associations, internal = FALSE, overwrite = TRUE)
#brand.associations <- matrix(as.numeric(as.matrix(z)), nrow = nrow(z),
#                             dimnames = dimnames(z))
