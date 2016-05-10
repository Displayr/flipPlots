csd.perceptions <- matrix(c(0.3004, 0.6864, 0.4975, 0.2908, 0.2781, 0.2642, 0.1916, 0.284, 0.3514, 0.2534, 0.2089,
c( 0.0198, 0.4604, 0.2151, 0.5235, 0.1151, 0.12, 0.5457, 0.3041, 0.06312, 0.384, 0.06064),
c( 0.01114, 0.4111, 0.1904, 0.4494, 0.06931, 0.1112, 0.4716, 0.2859, 0.0495, 0.3296, 0.03837),
c( 0.01114, 0.2373, 0.089, 0.2707, 0.05322, 0.06436, 0.2756, 0.1656, 0.02967, 0.1916, 0.02228),
c( 0.0198, 0.177, 0.07054, 0.0297, 0.0396, 0.02719, 0.0136, 0.02847, 0.0198, 0.02847, 0.02472),
c( 0.4543, 0.1275, 0.07673, 0.02847, 0.07293, 0.1077, 0.01609, 0.05198, 0.321, 0.01856, 0.0297),
c( 0.06807, 0.1089, 0.06064, 0.0198, 0.1174, 0.04084, 0.01609, 0.01733, 0.03465, 0.01361, 0.03589),
c( 0.08168, 0.224, 0.1015, 0.04579, 0.04815, 0.04084, 0.03094, 0.05562, 0.05322, 0.04084, 0.02847)),nrow=8,byrow=TRUE,
dimnames=list(Brand=c('Coke','V',"Red Bull","Lift Plus",'Diet Coke','Fanta','Lift','Pepsi'),
Attribute=c('Kids', 'Teens', "Enjoy life", 'Picks you up', 'Refreshes', 'Cheers you up', 'Energy', 'Up-to-date', 'Fun', 'When tired', 'Relax')))

devtools::use_data(csd.perceptions, internal = FALSE, overwrite = TRUE)


data(csd.perceptions)
z <- csd.perceptions[c(5:8, 1:4),3:4]

#dimnames(z)[[2]] <- c("col1","col2")#c("Dimension 1","Dimension 2")#paste0("`", dimnames(z)[[2]], "`")
#dimnames(z)[[1]] <- LETTERS[1:nrow(z)]

#csd.perceptions[,3:4]
LabeledScatterPlot(z, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = TRUE, fixed.aspect = TRUE, group = rep(1:2,4))

InteractiveLabeledScatterPlot(z, fixed.aspect = FALSE)

LabeledScatterPlot(z, font.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE, fixed.aspect = TRUE)
LabeledScatterPlot(z, font.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = TRUE, fixed.aspect = FALSE)
LabeledScatterPlot(z, font.size = 3, label.font.size = 10, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE, fixed.aspect = FALSE)

LabeledScatterPlot(z, auto.tidy = TRUE, fixed.aspect = FALSE, label.font.size = 20, font.size = 3, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12)
LabeledScatterPlot(z, auto.tidy = FALSE, fixed.aspect = FALSE, label.font.size = 20, font.size = 3, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12)

LabeledScatterPlot(z, font.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = TRUE)



LabeledScatterPlot(z, font.size = 3, label.font.size = 20, legend.font.size = 10, axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE)


z = matrix(c(1:10,1:10), 10, dimnames = list(c(rep("jJ", 3), rep("Big Dog", 3), rep("Stupendeously, Stupendeously big-canine", 4)), c("a1", "a2")))
LabeledScatterPlot(z, font.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE, fixed.aspect = FALSE)
LabeledScatterPlot(z, font.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = TRUE, fixed.aspect = FALSE)
LabeledScatterPlot(z, font.size = 3, label.font.size = 20, legend.font.size = 10,   axis.title.font.size = 12, axis.label.font.size = 10, title.font.size = 12, auto.tidy = FALSE, fixed.aspect = FALSE)

